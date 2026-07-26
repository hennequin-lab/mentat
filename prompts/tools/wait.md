Block until the named child sessions settle, then receive each child's final
result.

Children run detached: spawn returns immediately and the child keeps running
while you work. Its result reaches your context only through this call, so
name every child id you are blocked on in one wait — not one call per child. A
child that failed or was interrupted returns its failure or cancellation
message; waiting on one that already settled returns its recorded result
again.
