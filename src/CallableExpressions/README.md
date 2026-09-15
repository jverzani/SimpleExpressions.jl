# Vendored CallableExpressions core sources

This directory contains a vendored copy of the core source files from the Julia package `CallableExpressions`.

## Origin

The source in this directory was copied from the registered Julia package `CallableExpressions` (package UUID `391672e0-bbe4-4ab4-8bc9-b89a79cbc2f0`, version `1.1.1`) in the Julia General registry. It is kept here so that `SimpleExpressions` can use the core expression machinery without depending on the package's optional extension modules.

This vendored copy intentionally excludes the package's extension code under `ext/`, which is optional integration code for packages such as `AbstractTrees`, `ChainRulesCore`, and `TermInterface`.

## Attribution

`CallableExpressions` is authored by Neven Sajko and contributors.

Copyright (c) 2024 Neven Sajko <s@purelymail.com> and contributors

The package is distributed under the MIT License.

## License

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

## Reference

- JuliaHub: https://juliahub.com/ui/Packages/General/CallableExpressions
- Package source: the original package distributed via the Julia General registry
