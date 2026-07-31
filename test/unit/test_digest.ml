(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Windtrap
module Json = Jsont.Json

(* Pure helpers *)

let is_lower_hex c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')
let hex s = Mentat_digest.to_hex (Mentat_digest.string s)

(* Encode raw bytes to lowercase hex independently of the library, so the
   raw/hex agreement check does not compare the library against itself. *)
let hex_of_raw raw =
  String.concat ""
    (List.init (String.length raw) (fun i ->
         Printf.sprintf "%02x" (Char.code raw.[i])))

(* RFC 4648 base64url without padding, to reproduce the PKCE S256 challenge
   exactly the way the oauth2 consumer builds it from to_raw_string. *)
let base64url_no_pad s =
  let alphabet =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
  in
  let n = String.length s in
  let buf = Buffer.create ((n + 2) / 3 * 4) in
  let byte i = Char.code s.[i] in
  let emit bits = Buffer.add_char buf alphabet.[bits land 0x3f] in
  let i = ref 0 in
  while !i + 3 <= n do
    let b0 = byte !i and b1 = byte (!i + 1) and b2 = byte (!i + 2) in
    emit (b0 lsr 2);
    emit ((b0 lsl 4) lor (b1 lsr 4));
    emit ((b1 lsl 2) lor (b2 lsr 6));
    emit b2;
    i := !i + 3
  done;
  (match n - !i with
  | 1 ->
      let b0 = byte !i in
      emit (b0 lsr 2);
      emit (b0 lsl 4)
  | 2 ->
      let b0 = byte !i and b1 = byte (!i + 1) in
      emit (b0 lsr 2);
      emit ((b0 lsl 4) lor (b1 lsr 4));
      emit (b1 lsl 2)
  | _ -> ());
  Buffer.contents buf

(* The key framing oracle, mirroring the documented <decimal length>:<bytes>
   grammar. Kept independent of the implementation so the "full key = digest of
   framing" property is a real cross-check, not a tautology. *)
let add_frame buf text =
  Buffer.add_string buf (string_of_int (String.length text));
  Buffer.add_char buf ':';
  Buffer.add_string buf text

let framed ~domain parts =
  let buf = Buffer.create 128 in
  add_frame buf domain;
  List.iter (add_frame buf) parts;
  Buffer.contents buf

let expected_key ~domain parts = hex (framed ~domain parts)
let sign n = if n < 0 then -1 else if n > 0 then 1 else 0

let json_decode codec json =
  match Json.decode codec json with
  | Ok v -> v
  | Error m -> failf "decode failed: %s" m

let json_encode codec v =
  match Json.encode codec v with
  | Ok j -> j
  | Error m -> failf "encode failed: %s" m

let ok_ref = function
  | Ok v -> v
  | Error e -> failf "unexpected error: %s" (Mentat_digest.Error.message e)

(* Testables and generators *)

(* Bytes drawn so that content reliably contains NUL and high (invalid-UTF-8)
   bytes, and long enough (>64) to cross the SHA-256 block boundary. The
   explicit NUL weight makes that coverage dense and deterministic; the default
   uniform char generator hits NUL in only ~25% of generated strings. *)
let byte_gen =
  Gen.frequency
    [
      (1, Gen.pure '\000');
      (2, Gen.map Char.chr (Gen.int_range 128 255));
      (7, Gen.map Char.chr (Gen.int_range 32 126));
    ]

let content_gen = Gen.string_of ~size:(Gen.int_range 0 130) byte_gen
let digest = Testable.make ~pp:Mentat_digest.pp ~equal:Mentat_digest.equal

let digest_gen =
  Gen.with_pp Mentat_digest.pp (Gen.map Mentat_digest.string content_gen)

let content_ref =
  Testable.make ~pp:Mentat_digest.Content_ref.pp
    ~equal:Mentat_digest.Content_ref.equal

let content_ref_gen =
  Gen.with_pp Mentat_digest.Content_ref.pp
    (Gen.map Mentat_digest.Content_ref.of_contents content_gen)

let error =
  Testable.make
    ~pp:(fun ppf e ->
      Format.pp_print_string ppf (Mentat_digest.Error.message e))
    ~equal:( = )

let key_input_gen =
  Gen.bind (Gen.int_range 0 64) (fun length ->
      Gen.map
        (fun parts -> (length, parts))
        (Gen.list ~size:(Gen.int_range 0 4)
           (Gen.string_of ~size:(Gen.int_range 0 8) Gen.char)))

let key_input_gen =
  Gen.with_pp
    (fun ppf (length, parts) ->
      Format.fprintf ppf "(length=%d, [%s])" length
        (String.concat "; " (List.map (Printf.sprintf "%S") parts)))
    key_input_gen

(* Known constants *)

let empty_hex =
  "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"

let abc_hex = "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
let key_domain = "mentat.digest.test.v1"

(* SHA-256 known-answer vectors *)

let kat name input expected =
  test name (fun () -> equal string ~msg:name expected (hex input))

let known_answer_vectors =
  group "SHA-256 known-answer vectors"
    [
      kat "empty string" "" empty_hex;
      kat "abc" "abc" abc_hex;
      kat "quick brown fox" "The quick brown fox jumps over the lazy dog"
        "d7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592";
      kat "NIST 448-bit message"
        "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
        "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1";
      kat "NIST 896-bit message"
        "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"
        "cf5b16a778af8380036ce59e7b0492370b249b11e8f07a51afac45037afee9d1";
      kat "one million 'a' (multi-block)"
        (String.make 1_000_000 'a')
        "cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0";
    ]

(* Padding straddles the 64-byte block at 55/56 bytes; hold that boundary. *)
let padding_boundaries =
  group "SHA-256 padding boundaries"
    [
      kat "55 bytes (one block minus nine)" (String.make 55 'a')
        "9f4390f8d30c2dd92ec9f095b65e2b9ae9b0a925a5258e241c9f1e910f734318";
      kat "56 bytes (forces an extra block)" (String.make 56 'a')
        "b35439a4ac6f0948b6d6f9e3c6af0f5f590ce20f1bde7090ef7970686ec6738a";
      kat "57 bytes (spills into a second block)" (String.make 57 'a')
        "f13b2d724659eb3bf47f2dd6af1accc87b81f09f59f2b75e5c0bed6589dfe8c6";
      kat "64 bytes (exact block)" (String.make 64 'a')
        "ffe054fe7ae0cb6dc65c3af9b61d5209f439851db43d0ba5997337df154668eb";
      kat "65 bytes (two blocks)" (String.make 65 'a')
        "635361c48bb9eab14198e76ea8ab7f1a41685d6ad62aa9146d301d4f17eb0ae0";
    ]

(* Byte model: no Unicode / line-ending / NUL normalisation *)

let all_bytes = String.init 256 (fun code -> Char.chr code)

let byte_model =
  group "Byte model (no normalisation)"
    [
      test "hashes all 256 byte values exactly" (fun () ->
          equal string
            "40aff2e9d2d8922e47afd4648e6967497158785fbd1da870e7110266bf944880"
            (hex all_bytes));
      test "line endings are hashed as exact bytes" (fun () ->
          equal string
            "4edfe6fc73fd1015b423a47eb30b5ecb646706d5db6175d0f64f5af59eeac980"
            (hex "line\r\nnext\n"));
      test "an embedded NUL changes the digest" (fun () ->
          is_false
            (Mentat_digest.equal
               (Mentat_digest.string "ab")
               (Mentat_digest.string "a\000b")));
      test "hashing is deterministic" (fun () ->
          is_true
            (Mentat_digest.equal
               (Mentat_digest.string "mentat")
               (Mentat_digest.string "mentat")));
    ]

(* Serialisation laws *)

let serialisation_laws =
  group "Serialisation laws"
    [
      prop "to_hex is 64 lowercase hexadecimal characters" content_gen (fun s ->
          let h = hex s in
          equal int ~msg:"length" 64 (String.length h);
          String.iter
            (fun c ->
              if not (is_lower_hex c) then
                failf "non-lowercase-hex character: %C" c)
            h);
      prop "to_raw_string is exactly 32 bytes" content_gen (fun s ->
          equal int 32
            (String.length
               (Mentat_digest.to_raw_string (Mentat_digest.string s))));
      prop "raw bytes and hex encode the same digest" content_gen (fun s ->
          let d = Mentat_digest.string s in
          equal string (Mentat_digest.to_hex d)
            (hex_of_raw (Mentat_digest.to_raw_string d)));
      prop "pp renders to_hex" content_gen (fun s ->
          let d = Mentat_digest.string s in
          equal string (Mentat_digest.to_hex d)
            (Format.asprintf "%a" Mentat_digest.pp d));
    ]

(* to_raw_string / PKCE *)

let raw_and_pkce =
  group "to_raw_string / PKCE"
    [
      test "abc raw bytes are the hex-decoded digest" (fun () ->
          equal string abc_hex
            (hex_of_raw
               (Mentat_digest.to_raw_string (Mentat_digest.string "abc"))));
      test "raw digest bytes may contain NUL" (fun () ->
          (* The digest of "abc" contains a NUL byte (...f2 00 15 ad). *)
          is_true
            (String.contains
               (Mentat_digest.to_raw_string (Mentat_digest.string "abc"))
               '\000'));
      test "RFC 7636 verifier digest (hex)" (fun () ->
          equal string
            "13d31e961a1ad8ec2f16b10c4c982e0876a878ad6df144566ee1894acb70f9c3"
            (hex "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"));
      test "RFC 7636 PKCE S256 code challenge" (fun () ->
          equal string "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"
            (base64url_no_pad
               (Mentat_digest.to_raw_string
                  (Mentat_digest.string
                     "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"))));
    ]

(* Digest codec: of_hex *)

let of_hex_rejections =
  [
    ("empty", "");
    ("63 characters (too short)", String.take_first 63 abc_hex);
    ("65 characters (too long)", abc_hex ^ "0");
    ("all uppercase", String.uppercase_ascii abc_hex);
    ( "one uppercase character",
      String.take_first 63 abc_hex
      ^ String.uppercase_ascii (String.sub abc_hex 63 1) );
    ("non-hex letter g", String.take_first 63 abc_hex ^ "g");
    ("non-hex symbol", String.take_first 63 abc_hex ^ "!");
    ("embedded space", String.take_first 63 abc_hex ^ " ");
    ("leading space (length 64)", " " ^ String.take_first 63 abc_hex);
  ]

let of_hex_codec =
  group "Digest codec: of_hex"
    [
      test "accepts a known lowercase digest" (fun () ->
          equal (result digest pass)
            (Ok (Mentat_digest.string "abc"))
            (Mentat_digest.of_hex abc_hex));
      prop "round-trips to_hex over arbitrary bytes" content_gen (fun s ->
          (* Prove the interesting regions are actually exercised. *)
          cover ~label:"contains NUL" ~at_least:20.0 (String.contains s '\000');
          cover ~label:"contains a high byte (invalid UTF-8)" ~at_least:20.0
            (String.exists (fun c -> Char.code c >= 128) s);
          let d = Mentat_digest.string s in
          equal (result digest pass) (Ok d)
            (Mentat_digest.of_hex (Mentat_digest.to_hex d)));
      test "round-trips a NUL and invalid-UTF-8 digest" (fun () ->
          let d = Mentat_digest.string "a\000b\255\254\000\xf0\x28" in
          equal (result digest pass) (Ok d)
            (Mentat_digest.of_hex (Mentat_digest.to_hex d)));
      cases "rejects" ~name:fst of_hex_rejections (fun (_, input) ->
          equal (result digest error) (Error Mentat_digest.Error.Not_hex)
            (Mentat_digest.of_hex input));
    ]

(* Digest codec: jsont *)

let digest_json =
  group "Digest codec: jsont"
    [
      test "encodes as the to_hex JSON string" (fun () ->
          is_true
            (Json.equal (Json.string abc_hex)
               (json_encode Mentat_digest.jsont (Mentat_digest.string "abc"))));
      prop "round-trips a random digest" digest_gen (fun d ->
          equal digest d
            (json_decode Mentat_digest.jsont
               (json_encode Mentat_digest.jsont d)));
      test "decode rejects a non-hex string with a useful diagnostic" (fun () ->
          match Json.decode Mentat_digest.jsont (Json.string "bad") with
          | Ok d -> failf "accepted %a" Mentat_digest.pp d
          | Error m ->
              is_true ~msg:m
                (String.includes
                   ~affix:
                     (Mentat_digest.Error.message Mentat_digest.Error.Not_hex)
                   m));
      test "decode rejects a non-string JSON value" (fun () ->
          is_true
            (Result.is_error (Json.decode Mentat_digest.jsont (Json.int 1))));
    ]

(* Incremental hashing: Fold *)

let chunks_gen =
  Gen.with_pp
    (fun ppf cs ->
      Format.fprintf ppf "[%s]"
        (String.concat "; " (List.map (Printf.sprintf "%S") cs)))
    (Gen.list ~size:(Gen.int_range 0 6) content_gen)

let fold_of chunks =
  let ctx = Mentat_digest.Fold.create () in
  List.iter (Mentat_digest.Fold.add ctx) chunks;
  Mentat_digest.Fold.digest ctx

let incremental_hashing =
  group "Incremental hashing: Fold"
    [
      prop "folding chunks equals hashing their concatenation" chunks_gen
        (fun chunks ->
          cover ~label:"multiple chunks" ~at_least:20.0 (List.length chunks > 1);
          cover ~label:"contains NUL" ~at_least:20.0
            (List.exists (fun s -> String.contains s '\000') chunks);
          equal digest
            (Mentat_digest.string (String.concat "" chunks))
            (fold_of chunks));
      test "an empty stream hashes the empty string" (fun () ->
          equal digest (Mentat_digest.string "") (fold_of []));
      test "chunk boundaries do not move the digest" (fun () ->
          equal digest (fold_of [ "ab"; "c" ]) (fold_of [ "a"; "bc" ]));
      test "a NUL-bearing stream folds like a one-shot hash" (fun () ->
          equal digest
            (Mentat_digest.string "a\000b\000c")
            (fold_of [ "a\000"; "b"; "\000c" ]));
      test "add after digest raises" (fun () ->
          let ctx = Mentat_digest.Fold.create () in
          Mentat_digest.Fold.add ctx "abc";
          let (_ : Mentat_digest.t) = Mentat_digest.Fold.digest ctx in
          raises
            (Invalid_argument
               "Mentat_digest.Fold.add: context already finalized") (fun () ->
              Mentat_digest.Fold.add ctx "more"));
      test "digest after digest raises" (fun () ->
          let ctx = Mentat_digest.Fold.create () in
          let (_ : Mentat_digest.t) = Mentat_digest.Fold.digest ctx in
          raises
            (Invalid_argument
               "Mentat_digest.Fold.digest: context already finalized")
            (fun () -> ignore (Mentat_digest.Fold.digest ctx)));
    ]

(* Digest equality and order *)

let digest_order =
  group "Digest equality and order"
    [
      prop "equal and compare are reflexive" digest_gen (fun d ->
          is_true (Mentat_digest.equal d d && Mentat_digest.compare d d = 0));
      prop "compare = 0 agrees with equal" (Gen.pair digest_gen digest_gen)
        (fun (a, b) ->
          is_true (Mentat_digest.compare a b = 0 = Mentat_digest.equal a b));
      prop "compare sign is antisymmetric" (Gen.pair digest_gen digest_gen)
        (fun (a, b) ->
          is_true
            (sign (Mentat_digest.compare a b)
            = -sign (Mentat_digest.compare b a)));
      prop "compare is a transitive total order"
        (Gen.triple digest_gen digest_gen digest_gen) (fun (a, b, c) ->
          match List.sort Mentat_digest.compare [ a; b; c ] with
          | [ x; y; z ] ->
              is_true
                (Mentat_digest.compare x y <= 0
                && Mentat_digest.compare y z <= 0
                && Mentat_digest.compare x z <= 0)
          | _ -> ());
      test "distinct inputs give distinct digests" (fun () ->
          is_false
            (Mentat_digest.equal (Mentat_digest.string "a")
               (Mentat_digest.string "b")));
    ]

(* Derived identifiers: key *)

let differ ~msg a b = is_true ~msg (not (String.equal a b))
let full_key ~domain parts = Mentat_digest.key ~length:64 ~domain parts

let key_derivation =
  group "Derived identifiers: key"
    [
      test "single part is hashed with its domain frame" (fun () ->
          equal string
            (expected_key ~domain:key_domain [ "anchor" ])
            (full_key ~domain:key_domain [ "anchor" ]));
      test "zero length yields the empty string" (fun () ->
          equal string ""
            (Mentat_digest.key ~length:0 ~domain:key_domain [ "anchor" ]));
      test "length 64 equals the full framed digest" (fun () ->
          equal string
            (expected_key ~domain:key_domain [ "anchor" ])
            (Mentat_digest.key ~length:64 ~domain:key_domain [ "anchor" ]));
      test "no parts frames the domain alone" (fun () ->
          equal string
            (expected_key ~domain:key_domain [])
            (full_key ~domain:key_domain []));
      test "parts are length-framed" (fun () ->
          equal string
            (expected_key ~domain:key_domain [ "a"; "b" ])
            (full_key ~domain:key_domain [ "a"; "b" ]));
      test "split points are domain-separated" (fun () ->
          differ ~msg:{|["ab";"c"] vs ["a";"bc"]|}
            (full_key ~domain:key_domain [ "ab"; "c" ])
            (full_key ~domain:key_domain [ "a"; "bc" ]));
      test "an empty part differs from no parts" (fun () ->
          differ ~msg:{|[] vs [""]|}
            (full_key ~domain:key_domain [])
            (full_key ~domain:key_domain [ "" ]));
      test "an embedded NUL cannot erase a boundary" (fun () ->
          differ ~msg:{|["a";"b"] vs ["a\000b"]|}
            (full_key ~domain:key_domain [ "a"; "b" ])
            (full_key ~domain:key_domain [ "a\000b" ]));
      test "an embedded NUL cannot move a boundary" (fun () ->
          differ ~msg:{|["a";"b\000c"] vs ["a\000b";"c"]|}
            (full_key ~domain:key_domain [ "a"; "b\000c" ])
            (full_key ~domain:key_domain [ "a\000b"; "c" ]));
      test "distinct domains separate namespaces" (fun () ->
          differ ~msg:"domain a vs b"
            (full_key ~domain:"mentat.digest.test.a" [ "same" ])
            (full_key ~domain:"mentat.digest.test.b" [ "same" ]));
      prop "key has exactly length lowercase-hex characters" key_input_gen
        (fun (length, parts) ->
          let k = Mentat_digest.key ~length ~domain:key_domain parts in
          equal int ~msg:"length" length (String.length k);
          String.iter
            (fun c ->
              if not (is_lower_hex c) then
                failf "non-lowercase-hex character in key: %C" c)
            k);
      prop "key is a prefix of the full 64-character key" key_input_gen
        (fun (length, parts) ->
          equal string
            (String.take_first length (full_key ~domain:key_domain parts))
            (Mentat_digest.key ~length ~domain:key_domain parts));
      prop "full key equals the digest of the framed domain and parts"
        key_input_gen (fun (_length, parts) ->
          equal string
            (expected_key ~domain:key_domain parts)
            (full_key ~domain:key_domain parts));
      test "rejects a negative length" (fun () ->
          raises
            (Invalid_argument
               "Mentat_digest.key: length must be between 0 and 64") (fun () ->
              Mentat_digest.key ~length:(-1) ~domain:key_domain [ "anchor" ]));
      test "rejects a length above 64" (fun () ->
          raises
            (Invalid_argument
               "Mentat_digest.key: length must be between 0 and 64") (fun () ->
              Mentat_digest.key ~length:65 ~domain:key_domain [ "anchor" ]));
      test "rejects an empty domain" (fun () ->
          raises (Invalid_argument "Mentat_digest.key: empty domain") (fun () ->
              Mentat_digest.key ~length:8 ~domain:"" [ "anchor" ]));
    ]

(* Derived digests: derive *)

let derive_hex ~domain parts =
  Mentat_digest.to_hex (Mentat_digest.derive ~domain parts)

let frame_two a b =
  let buf = Buffer.create 32 in
  Mentat_digest.frame buf a;
  Mentat_digest.frame buf b;
  Buffer.contents buf

let framing_primitive =
  group "Framing primitive: frame"
    [
      test "frames as <decimal length>:<bytes>" (fun () ->
          let buf = Buffer.create 16 in
          Mentat_digest.frame buf "anchor";
          equal string "6:anchor" (Buffer.contents buf));
      test "frames an empty string as its zero length" (fun () ->
          let buf = Buffer.create 8 in
          Mentat_digest.frame buf "";
          equal string "0:" (Buffer.contents buf));
      test "an embedded colon does not blur the boundary" (fun () ->
          let buf = Buffer.create 16 in
          Mentat_digest.frame buf "a:b";
          equal string "3:a:b" (Buffer.contents buf));
      test "split points are separated" (fun () ->
          differ ~msg:{|["ab";"c"] vs ["a";"bc"]|} (frame_two "ab" "c")
            (frame_two "a" "bc"));
      prop "framing two byte strings is injective over the pair"
        (Gen.pair
           (Gen.pair content_gen content_gen)
           (Gen.pair content_gen content_gen))
        (fun ((a, b), (c, d)) ->
          equal bool ~msg:"framings agree iff the pairs agree"
            (String.equal a c && String.equal b d)
            (String.equal (frame_two a b) (frame_two c d)));
    ]

let derive_derivation =
  group "Derived digests: derive"
    [
      test "derive digests the framed domain and parts" (fun () ->
          equal string
            (expected_key ~domain:key_domain [ "anchor" ])
            (derive_hex ~domain:key_domain [ "anchor" ]));
      test "golden: single part is the digest of the hand-framed bytes"
        (fun () ->
          (* Framed by hand: "21:mentat.digest.test.v1" ^ "6:anchor". *)
          equal string
            "9b77df3b4fb06b2927ef2389d6b0e4be3566eeb755791163c0845c6b3673a86a"
            (hex "21:mentat.digest.test.v16:anchor");
          equal string
            "9b77df3b4fb06b2927ef2389d6b0e4be3566eeb755791163c0845c6b3673a86a"
            (derive_hex ~domain:key_domain [ "anchor" ]));
      test "golden: sandbox identity derivation stays byte-compatible"
        (fun () ->
          (* The sandbox identity digest frames its domain and parts with the
             same <decimal length>:<bytes> law; the derivation must keep
             producing those exact bytes. *)
          equal string
            "5eb60082fd887b2d9988623b099f340426ef07e65f11c7b6ecf81cf3d01dea47"
            (hex "26:mentat.sandbox.identity.v113:not_requested");
          equal string
            "5eb60082fd887b2d9988623b099f340426ef07e65f11c7b6ecf81cf3d01dea47"
            (derive_hex ~domain:"mentat.sandbox.identity.v1" [ "not_requested" ]));
      test "golden: no parts frames the domain alone" (fun () ->
          equal string
            "7e24b2e9d99c9e6b1f42bee0c5a22ded24b7f12fb51d696f91746abfde995bce"
            (derive_hex ~domain:key_domain []);
          equal string
            (expected_key ~domain:key_domain [])
            (derive_hex ~domain:key_domain []));
      test "golden: an empty part differs from no parts" (fun () ->
          equal string
            "623bc5e0ff3d68bece4f3f21939d007e94b8fa4b27718dba2dc8b437bdf67594"
            (derive_hex ~domain:key_domain [ "" ]);
          differ ~msg:{|[] vs [""]|}
            (derive_hex ~domain:key_domain [])
            (derive_hex ~domain:key_domain [ "" ]));
      test "split points are domain-separated" (fun () ->
          differ ~msg:{|["ab";"c"] vs ["a";"bc"]|}
            (derive_hex ~domain:key_domain [ "ab"; "c" ])
            (derive_hex ~domain:key_domain [ "a"; "bc" ]));
      test "distinct domains separate namespaces" (fun () ->
          differ ~msg:"domain a vs b"
            (derive_hex ~domain:"mentat.digest.test.a" [ "same" ])
            (derive_hex ~domain:"mentat.digest.test.b" [ "same" ]));
      prop "key is the truncated hex projection of derive" key_input_gen
        (fun (length, parts) ->
          equal string
            (String.take_first length (derive_hex ~domain:key_domain parts))
            (Mentat_digest.key ~length ~domain:key_domain parts));
      test "rejects an empty domain" (fun () ->
          raises (Invalid_argument "Mentat_digest.derive: empty domain")
            (fun () -> Mentat_digest.derive ~domain:"" [ "anchor" ]));
    ]

(* Content_ref: projections and matches *)

let content_ref_projections =
  group "Content_ref: projections and matches"
    [
      test "digest projection equals the string digest" (fun () ->
          is_true
            (Mentat_digest.equal
               (Mentat_digest.Content_ref.digest
                  (Mentat_digest.Content_ref.of_contents "abc"))
               (Mentat_digest.string "abc")));
      prop "digest projection equals the string digest (property)" content_gen
        (fun s ->
          equal digest
            (Mentat_digest.Content_ref.digest
               (Mentat_digest.Content_ref.of_contents s))
            (Mentat_digest.string s));
      test "digest hex is to_hex of the projection" (fun () ->
          equal string abc_hex
            (Mentat_digest.to_hex
               (Mentat_digest.Content_ref.digest
                  (Mentat_digest.Content_ref.of_contents "abc"))));
      test "length projection equals the byte length" (fun () ->
          equal int 3
            (Mentat_digest.Content_ref.length
               (Mentat_digest.Content_ref.of_contents "abc")));
      prop "length projection equals the byte length (property)" content_gen
        (fun s ->
          equal int
            (Mentat_digest.Content_ref.length
               (Mentat_digest.Content_ref.of_contents s))
            (String.length s));
      test "token is sha256:<hex>:<length>" (fun () ->
          equal string
            ("sha256:" ^ abc_hex ^ ":3")
            (Mentat_digest.Content_ref.to_token
               (Mentat_digest.Content_ref.of_contents "abc")));
      test "pp renders to_token" (fun () ->
          let r = Mentat_digest.Content_ref.of_contents "abc" in
          equal string
            (Mentat_digest.Content_ref.to_token r)
            (Format.asprintf "%a" Mentat_digest.Content_ref.pp r));
      test "matches accepts the exact content" (fun () ->
          is_true
            (Mentat_digest.Content_ref.matches
               (Mentat_digest.Content_ref.of_contents "abc")
               "abc"));
      prop "matches accepts the referenced content" content_gen (fun s ->
          is_true
            (Mentat_digest.Content_ref.matches
               (Mentat_digest.Content_ref.of_contents s)
               s));
      test "matches rejects a same-length one-byte edit" (fun () ->
          let s = "content-under-test" in
          let b = Bytes.of_string s in
          Bytes.set b 0 'X';
          is_false
            (Mentat_digest.Content_ref.matches
               (Mentat_digest.Content_ref.of_contents s)
               (Bytes.to_string b)));
      test "matches rejects a longer candidate" (fun () ->
          is_false
            (Mentat_digest.Content_ref.matches
               (Mentat_digest.Content_ref.of_contents "abc")
               "abcd"));
      test "matches rejects the empty string against non-empty content"
        (fun () ->
          is_false
            (Mentat_digest.Content_ref.matches
               (Mentat_digest.Content_ref.of_contents "abc")
               ""));
      test "verify accepts the exact content" (fun () ->
          match
            Mentat_digest.Content_ref.verify
              (Mentat_digest.Content_ref.of_contents "abc")
              "abc"
          with
          | Ok () -> ()
          | Error _ -> failf "expected Ok on the exact content");
      test "verify returns the actual reference on mismatch" (fun () ->
          let s = "content-under-test" in
          let b = Bytes.of_string s in
          Bytes.set b 0 'X';
          let edited = Bytes.to_string b in
          match
            Mentat_digest.Content_ref.verify
              (Mentat_digest.Content_ref.of_contents s)
              edited
          with
          | Ok () -> failf "expected Error on a mismatch"
          | Error actual ->
              is_true ~msg:"error carries the actual reference"
                (Mentat_digest.Content_ref.equal actual
                   (Mentat_digest.Content_ref.of_contents edited)));
      prop "verify is Ok exactly when matches is true" content_gen (fun s ->
          let r = Mentat_digest.Content_ref.of_contents "abc" in
          equal bool
            (Result.is_ok (Mentat_digest.Content_ref.verify r s))
            (Mentat_digest.Content_ref.matches r s));
    ]

(* Content_ref codec: of_token *)

let of_token_rejections =
  [
    ("empty", "");
    ("opaque string", "not-a-token");
    ("missing length", "sha256:" ^ abc_hex);
    ("empty length", "sha256:" ^ abc_hex ^ ":");
    ("extra field", "sha256:" ^ abc_hex ^ ":3:extra");
    ("wrong tag sha512", "sha512:" ^ abc_hex ^ ":3");
    ("wrong tag md5", "md5:" ^ abc_hex ^ ":3");
    ("missing tag", abc_hex ^ ":3");
    ("short hex (63)", "sha256:" ^ String.take_first 63 abc_hex ^ ":3");
    ("long hex (65)", "sha256:" ^ abc_hex ^ "0:3");
    ("uppercase hex", "sha256:" ^ String.uppercase_ascii abc_hex ^ ":3");
    ("non-hex digest", "sha256:" ^ String.take_first 63 abc_hex ^ "g:3");
    ("signed length", "sha256:" ^ abc_hex ^ ":+3");
    ("negative length", "sha256:" ^ abc_hex ^ ":-3");
    ("non-decimal length", "sha256:" ^ abc_hex ^ ":3a");
    ("leading-zero length", "sha256:" ^ abc_hex ^ ":03");
    ("trailing space in length", "sha256:" ^ abc_hex ^ ":3 ");
    ("space inside length", "sha256:" ^ abc_hex ^ ":3 4");
    ("hex-style length", "sha256:" ^ abc_hex ^ ":0x10");
    ("overflow length", "sha256:" ^ abc_hex ^ ":" ^ string_of_int max_int ^ "0");
  ]

let of_token_codec =
  group "Content_ref codec: of_token"
    [
      prop "round-trips to_token over arbitrary content" content_ref_gen
        (fun r ->
          equal (result content_ref pass) (Ok r)
            (Mentat_digest.Content_ref.of_token
               (Mentat_digest.Content_ref.to_token r)));
      test "zero-length content round-trips" (fun () ->
          let r = Mentat_digest.Content_ref.of_contents "" in
          equal string
            ("sha256:" ^ empty_hex ^ ":0")
            (Mentat_digest.Content_ref.to_token r);
          equal (result content_ref pass) (Ok r)
            (Mentat_digest.Content_ref.of_token
               (Mentat_digest.Content_ref.to_token r)));
      test "accepts a max_int length" (fun () ->
          match
            Mentat_digest.Content_ref.of_token
              ("sha256:" ^ abc_hex ^ ":" ^ string_of_int max_int)
          with
          | Ok r -> equal int max_int (Mentat_digest.Content_ref.length r)
          | Error e ->
              failf "max_int length rejected: %s"
                (Mentat_digest.Error.message e));
      cases "rejects" ~name:fst of_token_rejections (fun (_, input) ->
          equal (result content_ref error)
            (Error Mentat_digest.Error.Malformed_content_ref)
            (Mentat_digest.Content_ref.of_token input));
    ]

(* Content_ref codec: jsont *)

let content_ref_json =
  group "Content_ref codec: jsont"
    [
      test "encodes as the to_token JSON string" (fun () ->
          is_true
            (Json.equal
               (Json.string ("sha256:" ^ abc_hex ^ ":3"))
               (json_encode Mentat_digest.Content_ref.jsont
                  (Mentat_digest.Content_ref.of_contents "abc"))));
      prop "round-trips a random reference" content_ref_gen (fun r ->
          equal content_ref r
            (json_decode Mentat_digest.Content_ref.jsont
               (json_encode Mentat_digest.Content_ref.jsont r)));
      test "decode rejects a malformed token with a useful diagnostic"
        (fun () ->
          match
            Json.decode Mentat_digest.Content_ref.jsont
              (Json.string "not-a-token")
          with
          | Ok r -> failf "accepted %a" Mentat_digest.Content_ref.pp r
          | Error m ->
              is_true ~msg:m
                (String.includes
                   ~affix:
                     (Mentat_digest.Error.message
                        Mentat_digest.Error.Malformed_content_ref)
                   m));
      test "decode rejects a non-string JSON value" (fun () ->
          is_true
            (Result.is_error
               (Json.decode Mentat_digest.Content_ref.jsont (Json.int 1))));
    ]

(* Content_ref: equality and order.

   of_token builds references with a chosen digest and length independently, so
   equality can be probed on each field in isolation. *)

let content_ref_order =
  group "Content_ref: equality and order"
    [
      test "equality requires matching digest and length" (fun () ->
          let r_abc_10 =
            ok_ref
              (Mentat_digest.Content_ref.of_token ("sha256:" ^ abc_hex ^ ":10"))
          in
          let r_abc_11 =
            ok_ref
              (Mentat_digest.Content_ref.of_token ("sha256:" ^ abc_hex ^ ":11"))
          in
          let r_empty_10 =
            ok_ref
              (Mentat_digest.Content_ref.of_token
                 ("sha256:" ^ empty_hex ^ ":10"))
          in
          is_false ~msg:"same digest, different length"
            (Mentat_digest.Content_ref.equal r_abc_10 r_abc_11);
          is_false ~msg:"different digest, same length"
            (Mentat_digest.Content_ref.equal r_abc_10 r_empty_10);
          is_true ~msg:"identical fields"
            (Mentat_digest.Content_ref.equal r_abc_10
               (ok_ref
                  (Mentat_digest.Content_ref.of_token
                     ("sha256:" ^ abc_hex ^ ":10"))));
          is_true ~msg:"length disagreement orders"
            (Mentat_digest.Content_ref.compare r_abc_10 r_abc_11 <> 0);
          is_true ~msg:"digest disagreement orders"
            (Mentat_digest.Content_ref.compare r_abc_10 r_empty_10 <> 0));
      prop "equal and compare are reflexive" content_ref_gen (fun r ->
          is_true
            (Mentat_digest.Content_ref.equal r r
            && Mentat_digest.Content_ref.compare r r = 0));
      prop "compare = 0 agrees with equal"
        (Gen.pair content_ref_gen content_ref_gen) (fun (a, b) ->
          is_true
            (Mentat_digest.Content_ref.compare a b
            = 0
            = Mentat_digest.Content_ref.equal a b));
      prop "compare sign is antisymmetric"
        (Gen.pair content_ref_gen content_ref_gen) (fun (a, b) ->
          is_true
            (sign (Mentat_digest.Content_ref.compare a b)
            = -sign (Mentat_digest.Content_ref.compare b a)));
      prop "compare is a transitive total order"
        (Gen.triple content_ref_gen content_ref_gen content_ref_gen)
        (fun (a, b, c) ->
          match List.sort Mentat_digest.Content_ref.compare [ a; b; c ] with
          | [ x; y; z ] ->
              is_true
                (Mentat_digest.Content_ref.compare x y <= 0
                && Mentat_digest.Content_ref.compare y z <= 0
                && Mentat_digest.Content_ref.compare x z <= 0)
          | _ -> ());
    ]

(* Errors *)

let errors =
  group "Errors"
    [
      test "Not_hex message" (fun () ->
          equal string
            "digest must be exactly 64 lowercase hexadecimal characters"
            (Mentat_digest.Error.message Mentat_digest.Error.Not_hex));
      test "Malformed_content_ref message" (fun () ->
          equal string
            "content reference must be sha256:<64 lowercase hex>:<length>"
            (Mentat_digest.Error.message
               Mentat_digest.Error.Malformed_content_ref));
      test "pp matches message (Not_hex)" (fun () ->
          equal string
            (Mentat_digest.Error.message Mentat_digest.Error.Not_hex)
            (Format.asprintf "%a" Mentat_digest.Error.pp
               Mentat_digest.Error.Not_hex));
      test "pp matches message (Malformed_content_ref)" (fun () ->
          equal string
            (Mentat_digest.Error.message
               Mentat_digest.Error.Malformed_content_ref)
            (Format.asprintf "%a" Mentat_digest.Error.pp
               Mentat_digest.Error.Malformed_content_ref));
    ]

let () =
  run "mentat.digest"
    [
      known_answer_vectors;
      padding_boundaries;
      byte_model;
      serialisation_laws;
      raw_and_pkce;
      of_hex_codec;
      digest_json;
      incremental_hashing;
      digest_order;
      key_derivation;
      framing_primitive;
      derive_derivation;
      content_ref_projections;
      of_token_codec;
      content_ref_json;
      content_ref_order;
      errors;
    ]
