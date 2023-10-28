<h3 align="center">WasmEdge-Haskell</h3>

<div align="center">

## Haskell binding to [WasmEdge](https://wasmedge.org/) Runtime for hosting


  [![Status](https://img.shields.io/badge/status-active-success.svg)]() 
  [![GitHub Issues](https://img.shields.io/github/issues/kylelobo/The-Documentation-Compendium.svg)](https://github.com/byteally/wasmedge/issues)
  [![GitHub Pull Requests](https://img.shields.io/github/issues-pr/kylelobo/The-Documentation-Compendium.svg)](https://github.com/byteally/wasmedge/pulls)
  [![License](https://img.shields.io/badge/License-BSD_3--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

</div>

## Introduction
WasmEdge-Haskell is a library that allows you to use WebAssembly code (.wasm) in your Haskell projects at near-native speed. It's a Haskell binding for the WasmEdge [C-SDK](https://wasmedge.org/docs/embed/c/intro), allowing you to use Haskell as a host application and embed WebAssembly functions written in other languages.

## Getting Started

### Prerequisites
1. Install WasmEdge

        $ curl -sSf https://raw.githubusercontent.com/WasmEdge/WasmEdge/master/utils/install.sh | bash

2. Clone this repository into your Haskell project.

        with-compiler: ghc-9.2.8

        source-repository-package
        type: git
        location: git@github.com:byteally/wasmedge.git
        tag: 80f3635a2b3f5346c85ef55bd599b081ebc995d8 -- use latest commit tag here

        packages:
            <project-name>.cabal

3. Specify the package in your project's `.cabal` file as a dependency.

        build-depends:    base ^>= 4.11.1.0
                          ,wasmedge

## 🎈 Usage <a name="usage"></a>
Below is an example of using the `vmRunWasmFromFile` function, assuming you have a `addTwo.wasm` file which contains a function named `addTwo`.

```Haskell
{-# LANGUAGE OverloadedStrings #-} -- for conversion of String and WasmString
module Main where

import WasmEdge.Configure
import WasmEdge.Internal.FFI.Bindings (HostRegistration(HostRegistration_Wasi),WasmVal(WasmInt32))
import WasmEdge.VM
import qualified Data.Vector as V

addTwoNumbers :: Int -> Int -> IO ()
addTwoNumbers x y = do
    Just cfg <- configureCreate
    configureAddHostRegistration cfg HostRegistration_Wasi  --optional
    Just vm <- vmCreate (Just cfg) Nothing
    addTwoRes <- vmRunWasmFromFile vm "./wasm/addTwo.wasm" "addTwo" (V.fromList [WasmInt32 (fromIntegral x),WasmInt32 (fromIntegral y)]) 1
    print (snd addTwoRes)

main :: IO ()
main = addTwoNumbers 2 3
```

### Output
```bash
[5]
```

## API Reference
The API Documentation would be available on Hackage soon.

## Reference
For more information, please refer to the WasmEdge's [Embed](https://wasmedge.org/docs/embed/overview) Documentation.

## ✍️ Authors <a name = "authors"></a>
- [@mageshb](https://github.com/mageshb) - Idea & Initial work
- [@tusharad](https://github.com/tusharad) - Contributed

## Contribution

Contributions are welcome! Feel free to fork the repository or submit a pull request.