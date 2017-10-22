Versioning
==========

- [why](#why)
- [examples](#examples)
- [corba](#corba)
- [corba valiation](#corba-validation)


## Why?

Supporting some form of versioning is an important mechanism for ensuring that
client/services can communicate reliability and that services can be upgraded
to newer versions with new functionality without breaking old behaviour (for
some length of time).

Some examples of breaking compatibility are:

- Removing fields that previously existed
- Changing the type of fields (ie from a string to an int)

Some general techniques for versioning are:

- An out-of-band version (tag/string/number) that indicate how a given
  sequence of bytes can be interpreted.

  In the case of HTTP this could be the `Content-Type` request/response header
  that can be used to parse the following body string.

- Schema evolution

  This usually takes the form of optional or new fields.

  In the case of JSON this might be an object field that is missing or `null`,
  or perhaps a new "type" that represents a new ADT constructor.


## Examples

All RPC frameworks (that I can find) use the latter and rely on schema
evolution.

- https://martin.kleppmann.com/2012/12/05/schema-evolution-in-avro-protocol-buffers-thrift.html
- https://github.com/bkayser/thrift-versioning-doc#backward-compatible-changes


## Corba

Corba's transport uses the standard HTTP accept/content-type headers to
handle versioning of the internal JSON structure. This can/will be used to
handle any changes to the internal JSON formatting (ie. the rules for how
json is generated from machinator change in a breaking way).

Corba uses schema evolution for data versioning. In particular it will support
this via new machinator constructors.
In future it is almost certainly going to be handy/nice to add specific support
for `Maybe` rather than having to copy entire constructors/records every time.

### V1

```haskell
data Input =
    InputV1 String

data Output =
    OutputV1

service Service = {
    method1: Input -> Output
  }
```

### V2 - Change data

A new contructor will need to be added with the new value.

```haskell
data Input =
    InputV1 String
  | InputV2 String Int
```

### V3 - Change data (breaking)

In some cases the behaviour of a method may change in a way that requires
both the input _and_ output data to change. In this case it is recommended to
create a new method and input/output to avoid having to deal with the
cross-product of constructor cases that aren't possible.


## Corba - Validation

In the beginning Corba won't support any tool-driven validation for spotting
breaking changes. However it is desired that at some point we introduce a
corba CLI tool for inspecting the old and new IDL files that encode the
hard-and-fast rules for safe versioning. This could be done in a number of
ways.

- use `git` to lookup the previous version
- keep all versions around forever
- publish version to an S3 dispensary

For now validation will be done, as it previously was, via pull requests
and testing.
