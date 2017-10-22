Motivation
==========

- Dependency hell: regenerate the code and check it in, no shared
  code, very limited dependency base
- Marshalling: all transport code is generated, just write your
  business logic as if services were libraries
- Unsafe API versioning: we can analyse service definitions over time
  and ensure breaking changes do not happen accidentally
- Client libraries: generated for free
- REST services: don't write them anymore, just write business logic

Our requirements are also pretty limited:
- All our services are Haskell or Purescript
- Any other language ecosystems would be a significant and intentional
  investment on our part, so we'd be able to write codegen
- We don't care about concise message formats (we use ad-hoc JSON now)
- We don't care about asynchronous RPC
- We don't care about streaming

_The aim here is to get to where we want to be as fast as
possible_. Corba thus has a much smaller scope than off-the-shelf
service definition frameworks like Swagger and gRPC. We may choose to
migrate to such frameworks in the future, but for now it is much
faster to roll ourselves.
