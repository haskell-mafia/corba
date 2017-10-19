# corba

![screen shot 2017-10-18 at 3 12 30 pm](https://user-images.githubusercontent.com/355756/31700782-5d2a75da-b417-11e7-8ed7-6dd412605367.png)

> "It sounds an awful lot like you're reinventing CORBA" - Pierce, 2017

Corba is an extremely simple, naive RPC system. It is intended to
automate a bunch of busywork around Ambiata service communication.

Given a service definition and a set of Machinator datatypes, it will
generate Haskell datatypes, message codecs (JSON for now), WAI
middleware, a client library, and documentation. This addresses a
bunch of process problems:

- [motivation](doc/motivation.md)


## Example

Service definition:

```haskell
Service {
  serviceName = ServiceName "greeting"
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

-- (Ditto for the Goodbye Request
--  and Response definitions.)
```

Generated abstract interface:

```haskell
data GreetingService m = GreetingService {
    hello :: HelloRequest -> m HelloResponse
  , goodbye :: GoodbyeRequest -> m GoodbyeResponse
  }
```

Generated WAI middleware:

```haskell
greetingMiddleware :: Route -> GreetingService (ExceptT ErrorMessage IO) -> (Wai.Application -> Wai.Application)
```

Generated client:

```haskell
greetingClient :: Monad m => GreetingService (HttpApi m)
```
