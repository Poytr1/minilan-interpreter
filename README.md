# minilan-interpreter

## 准备(默认已安装GHC)
1. 安装cabal(Windows直接可以直接下载[cabal.exe](https://www.haskell.org/cabal/release/cabal-install-latest/))，添加环境变量例如将其放到**C:\Program Files\Haskell\bin**下。然后，获取cabal的最新版本:
    ```shell
     $ cabal update
    ```
2. 下载程序所需依赖:
    ```shell
    $ cabal install parsec
    $ cabal install lens
    ... (可能存在其他依赖)
    ```

## 编译
```shell
$ ghc --make Main.hs -o minilan
```
## 执行
```shell
$ ./minilan
```
## 测试用例
*Test* 文件夹里提供了四个测试用例，注意测试的时候需要将文件重命名为**program.txt**。输入的值从 *input.txt* 中读取，程序结果输出到 *output.txt* 中。

1. *program.txt* 中初始示例是一个闭包情景。输入:无  输出: 1 3
2. *test1.txt* 代码为递归计算阶乘的示例。输入:n  输出:n!
3. *test2.txt* 代码为高阶函数示例。 输入:无  输出: 19
4. *test3.txt* 代码为循环直接计算阶乘的示例，测试while循环等基础功能。
   输入:n  输出:n!
5. *test4.txt* 代码为currying情景。输入:x  输出: x+3
6. *test5.txt* 代码为nested program的情景。输入:无 输出:0 1 1

## GC算法简介
考虑到该语言语法上比较简单存在需要Garbage Collector的地方主要是函数的调用返回阶段：

1. 函数不返回对另外函数的引用，对于这种情况处理情况比较简单：在函数调用的时候为其分配Activity Record并为函数声明的形参与局部变量分配空间，在函数执行完时回收AR以及变量空间。

2. 函数返回对函数的引用，这种情况可以归一到上一种情况，在返回调用时进入其引用函数的作用域为其分配AR与变量空间。等到引用的函数结束，获取值，释放引用函数空间。返回值，最后释放自己的AR与变量空间。

具体结果可以参照 *test1.txt* 用例的输出，其中递归调用fact函数自身时一一分配空间，返回时释放空间。(结果在控制台输出)
