# corba

![screen shot 2017-10-18 at 3 12 30 pm](https://user-images.githubusercontent.com/355756/31700782-5d2a75da-b417-11e7-8ed7-6dd412605367.png)

> "It sounds an awful lot like you're reinventing CORBA" - Pierce, 2017

Corba is an extremely simple, naive RPC system. It is intended to
automate a bunch of busywork around Ambiata service communication.

Given a service definition and a set of Machinator datatypes, it will
generate Haskell datatypes, message codecs (JSON for now), WAI
middleware, a client library, and documentation. This addresses a
bunch of process problems:

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

The aim here is to get to where we want to be as fast as
possible. Corba thus has a much smaller scope than off-the-shelf
service definition frameworks like Swagger and gRPC. We may choose to
migrate to such frameworks in the future, but for now it is much
faster to roll ourselves.

## Example

Service definition:

```haskell
Service {
  serviceName = ServiceName "hello"
, serviceMethods = [
    Method {
        methodName = MethodName "hello"
      , methodRequest = TypeName "HelloRequest"
      , methodResponse = TypeName "HelloResponse"
      }
  , Method {
        methodName = MethodName "goodbye"
      , methodRequest = TypeName "GoodbyeRequest"
      , methodResponse = TypeName "GoodbyeResponse"
      }
  ]
}
```

Data definition:

```haskell
data HelloRequest =
    HelloRequestV1 String
  | HelloRequestV2 String Message

data HelloResponse =
    HelloResponseV1 ResponseMsg

record Message = {
  message : String
}

data ResponseMsg =
    Worked
  | Failed
```

Generated abstract interface:

```haskell
data HelloService m = HelloService {
    hello :: HelloRequest -> m HelloResponse
  }
```

Generated WAI middleware:

```haskell
helloMiddleware :: Route -> HelloService (ExceptT ErrorMessage IO) -> (Wai.Application -> Wai.Application)
```

Generated client:

```haskell
helloClient :: Monad m => HelloService (HttpApi m)
```
