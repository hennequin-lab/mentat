#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <stdint.h>
#include <string.h>

#include "sha256.h"

CAMLprim value caml_mentat_digest_sha256_string(value input)
{
  CAMLparam1(input);
  CAMLlocal1(output);
  struct sha256_ctx ctx;
  mlsize_t len = caml_string_length(input);

  output = caml_alloc_string(SHA256_DIGEST_SIZE);
  const uint8_t *data = (const uint8_t *)String_val(input);
  mentat_sha256_init(&ctx);
  while (len > 0) {
    uint32_t chunk =
        len > (mlsize_t)UINT32_MAX ? UINT32_MAX : (uint32_t)len;
    mentat_sha256_update(&ctx, data, chunk);
    data += chunk;
    len -= chunk;
  }
  mentat_sha256_finalize(&ctx, (uint8_t *)Bytes_val(output));
  CAMLreturn(output);
}

/* Incremental hashing. The [struct sha256_ctx] lives inside an OCaml [bytes]
   the caller sizes with [ctx_size]; word-aligned OCaml block data satisfies the
   struct's [uint64_t] alignment. No stub below allocates while holding a raw
   pointer into a context or input, so no GC can move the bytes mid-op. */

CAMLprim value caml_mentat_digest_sha256_ctx_size(value unit)
{
  CAMLparam1(unit);
  CAMLreturn(Val_long(SHA256_CTX_SIZE));
}

CAMLprim value caml_mentat_digest_sha256_fold_init(value ctx)
{
  CAMLparam1(ctx);
  mentat_sha256_init((struct sha256_ctx *)Bytes_val(ctx));
  CAMLreturn(Val_unit);
}

CAMLprim value caml_mentat_digest_sha256_fold_add(value ctx, value input)
{
  CAMLparam2(ctx, input);
  struct sha256_ctx *c = (struct sha256_ctx *)Bytes_val(ctx);
  mlsize_t len = caml_string_length(input);
  const uint8_t *data = (const uint8_t *)String_val(input);
  while (len > 0) {
    uint32_t chunk =
        len > (mlsize_t)UINT32_MAX ? UINT32_MAX : (uint32_t)len;
    mentat_sha256_update(c, data, chunk);
    data += chunk;
    len -= chunk;
  }
  CAMLreturn(Val_unit);
}

CAMLprim value caml_mentat_digest_sha256_fold_digest(value ctx)
{
  CAMLparam1(ctx);
  CAMLlocal1(output);
  output = caml_alloc_string(SHA256_DIGEST_SIZE);
  mentat_sha256_finalize((struct sha256_ctx *)Bytes_val(ctx),
                        (uint8_t *)Bytes_val(output));
  CAMLreturn(output);
}
