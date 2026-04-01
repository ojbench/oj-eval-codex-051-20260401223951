# Problem 051 - Yx Compiler!

**ACMOJ Problem ID**: 1272

## Table of Contents

- [Problem 051 - Yx Compiler!](#problem-051-yx-compiler)
  - [Table of Contents](#table-of-contents)
  - [Introduction](#introduction)
    - [Background](#background)
  - [Assignment Description](#assignment-description)
    - [Grade Composition](#grade-composition)
  - [Assignment Requirements](#assignment-requirements)
    - [Input Format](#input-format)
    - [Output Format](#output-format)
    - [Samples](#samples)
    - [Data Constraints](#data-constraints)
  - [Per-Testcase Resource Limits](#per-testcase-resource-limits)
  - [Test Data](#test-data)
  - [Submission Requirements](#submission-requirements)
    - [OJ Git Repository Compilation Process](#oj-git-repository-compilation-process)
    - [Git Configuration Requirements](#git-configuration-requirements)
    - [Submission Guidelines](#submission-guidelines)
    - [Evaluation Notes](#evaluation-notes)
    - [Academic Integrity](#academic-integrity)

## Introduction

### Background

Simplified compiler: address translation (.dmp to .tis) and register allocation optimization.

## Assignment Description

### Grade Composition

| Grading Component | Percentage |
| :--: | :--: |
| Pass **1272. Yx Compiler!** | 80% |
| Code Review | 20% |

Here are several points that need clarification:

- In the Code Review, we will **strictly examine your code style and repository organization structure, etc.**

- This assignment provides some sample data for testing, stored in the `/workspace/data/051/data_test/` directory. Note that these are not the test cases on the Online Judge. Passing all local test cases does not guarantee that you will pass the OJ tests.

- Besides the provided sample data, we also encourage you to design your own test data based on your program logic to assist debugging.

## Assignment Requirements

### Problem Description

### 助教的话

众所周知，编译器是 ACM 班所有同学必须挺过的难关。在去年的 PPCA 中，就有一个叫做 Basic Compiler 的项目可以供大家选择，用于提前熟悉编译器的相关知识。

很可惜的是，这样一个用心良苦的项目并不受我们待见。当时选择了（被分配）这一个项目的同学大都没能够完成一个高质量的编译器。以至于我们现在将这一个项目从 PPCA 中移除了。

对于去年 PPCA 编译器项目感兴趣的同学可以查看 [这个仓库](https://github.com/Anoxiacxy/BASIC/tree/master/test)。当然，考试期间请不要在 github 上闲逛！

所以最终，我们做了一个违背祖宗的决定，将编译器这一个项目放入机考之中！

### 任务描述

很显然，在短短的 5 个小时时间里面，你是不可能完成一个编译器的。所以我们将任务得以简化。

- 任务一：相对地址到绝对地址的转换，将 `*.dmp` 文件进行转化后输出到 `*.tis` 的可执行文件中
- 任务二：寄存器分配，内存访问次数优化，因为我们提供的 `*.dmp` 文件中使用的指令集将大量访问内存

### TISC 简介

Three Instruction Set Computing (TISC) 和我们曾经了解的 Reduced Instruction Set Computing (RISC) 有着类似的概念。这里有内存 (Reg)，寄存器 (Mem)，程序计数器 (Pc)。如同 TISC 这个名字所说的，这种计算机模型中只有 3 条不同的指令，分别是 `msubleq`，`rsubleq`，`ldorst`。每个指令都有 3 个参数，依次是 `a`, `b`, `c`。

#### 存储模型

为了方便起见，我们统一了所有存储单元的单位空间，均为 `32 bits` 的大小。简单而言，你理解为我们的存储空间全部都是 `int` 数组：

```c
int Mem[0xffffff];
int Reg[32];
int Pc = 0;
```

一条指令就由 4 部分组成，可以写作：

> `opt a b c;`

指令将会以机器码的形式存储在**内存**中，每条指令占据内存中的 4 个单位的空间。为了方便起见，我们这里的内存单位空间就是一个 `int` 的大小，即 `32 bits`。所以一个指令需要 4 个 `int` 来存储，也就是我们的指令会以以下方式存储在内存中：

```
msubleq a1 b1 c1;  |  Mem[0]=msubleq  Mem[1]=a1  Mem[2]=b1  Mem[3]=c1
ldorst  a2 b2 c2;  |  Mem[4]=ldorst   Mem[5]=a2  Mem[6]=b2  Mem[7]=c2
rsubleq a3 b3 c3;  |  Mem[8]=rsubleq  Mem[9]=a3  Mem[10]=b3 Mem[11]=c3
```

我们的计算机执行的时候，会从 `Pc` 开始，依次读取 `Men[Pc], Mem[Pc+1], Mem[Pc+2], Mem[Pc+3]` 的内容，将其解释为一条指令进行执行。如果执行该指令的过程中没有跳转操作，那么 `Pc=Pc+4`，也就是顺次执行接下来内存中的指令。直到 `Pc<0`，结束运行。

#### 指令集

TISC 拥有一个比 RISC 要精简的多的指令集系统。这三条指令的解释如下：

- `rsubleq a b c;`  如果寄存器相减后不大于〇就跳转。
```c
Reg[b] -= Reg[a];
if (Reg[b] <= 0) 
    Pc = c;
else
    Pc += 4;
```

- `ldorst a b c;` 将数据加载到寄存器或者保存到内存。
```c
if (c <= 0) 
    reg[b] = mem[a];
else
    mem[b] = reg[a];
```

- `msubleq a b c;` 如果内存相减后不大于〇就跳转。
```c
Mem[b] -= Mem[a];
if (Mem[b] <= 0) 
    Pc = c;
else
    Pc += 4;
```

当然，对于**读入和输出**操作，也被集成到 `msubleq` 指令里了。
- 当 `a < 0` 的时候，就是从标准输入读取一个 ascii 字符放入 `Mem[b]` 中
- 当 `b < 0` 的时候，就是将 `Mem[a]` 以 ascii 字符的形式输出到标准输出

#### 可执行文件 `.tis`

该文件中由若干纯数字组成，按顺序的第 `i` 个数字代表了 `Mem[i-1]` 的初值。在 `.tis` 格式文件中，指令 `msubleq` 会被编码为 `0`，`rsubleq` 会被编码为 `1`，`ldorst` 会被编码为 `2`。

#### 可阅读的汇编文件 `.dmp`

该文件包含了一份汇编程序，语法结构如下：

```
program	:= intruction*
intruction := opcode (item)* ';'
opcode  := (label:)* ( '.' | msubleq | rsubleq | ldorst )
item    := (label:)* expression
expression := ( term | term+term | term-term )
term 	:= ( -term | (expression) | const | id )
const	:= ( number | '?' )
label	:= id
```

- 几句题外话：可能有同学在写RISCV模拟器时就会产生困惑，为什么给出的dmp文件中的汇编我看不懂？难道不是RV32I的这些opcode类型可以与汇编指令的类型一一对应吗？不是这样的。事实上，汇编是一种给人看的语言，因此有些汇编指令代表了一些更高级的操作也不足为奇。也就是说，一条dmp中的指令，如果真的要翻译到tis，可能会转化为1条，多条，甚至是0条指令……不过幸运的是，本次任务中dump文件也只会出现四种指令，除了可以与机器码一一对应的``msubleq`` ，``rsubleq``，``ldorst``以外，还有一种指令叫``.``。至于``.``是干什么的，下面会详细解释，但可以提前给你剧透，**.代表的指令是不会被翻译出来出现在tis文件里的！**

- ``label``

  - ``label``的中文意思是标签，如果要类比的话，你可以认为``label``是一个**指针**。不过`label` 不占用任何内存空间，因为``label``是只存在于汇编dmp里的，给人看的，方便人理解的一个东西；它可以被别处引用，作为 `label` 所在的地址。例如：

    ```text
    top:
    rsubleq 0 0 tismain;
    
    tismain:
        msubleq dec sp;      //tismain是指向这一条指令的指针
        ......
    ```
    这里的 tismain 就是一个 label，在 top 所在的指令处引用这一个 label，相当于是把 tismain 中指令的起始地址保存在了 top 指令中。这样 Pc 就可以直接跳转到 tismain 的地方开始执行。
- `opcode` 
  - 如果是 `msubleq, rsubleq, ldorst`，那么被翻译成 `.tis` 的时候直接替换为 `0, 1, 2` 需要占据一个内存空间
    
    关于 `msubleq` 和 `rsubleq` 指令，如果参数的数量不足 3，语法会认为自动拓展后续的参数。
    ```text
    msubleq A;      |   msubleq A A ?;
    msubleq A B;    |   msubleq A B ?;
    msubleq A B C;  |   msubleq A B C;
    ```
    按照这样的规则拓展后，一个参数就代表清空 A 内存地址的内容（`Mem[A] = 0;`）
    两个个参数就代表 `Mem[B] -= Mem[A]` 但是无论如何都不跳转。
  - 如果是 `.` ，说明这里仅仅保存了一些常量数据，不需要 `opcode` ，该字符不占用任何内存空间
    ```text
    . inc:-1 Z:0 dec:1 ax:0 bp:0 sp:-sp;
    ```
    这里相当于是定义了一些常量，在 subleq 这类指令中，想要增加 `1` 的操作是减去 `-1`，所以我们在 inc 这个label指向的内存空间保存了一个 `-1`。
    解释 | sp:-sp  首先，sp 是一个标签，本质上的值是一个是一个内存地址。 显然sp 这个标签的位置是确定的，那么这个值就是确定的。 sp:-sp 表示在当前地址的内存中保存当前地址的负值。
- `item` 一定会占用一个内存空间

- `?` 代表了所在 `item` 的地址 +1
    ```text
    tismain:
        msubleq dec sp;
        msubleq ?+15;
        msubleq sp ?+10;
        msubleq ?+8;
        msubleq sp ?+3;
        msubleq 0;
        msubleq ?+8;
        msubleq sp ?+3;
        msubleq ?+2 0 _main;
        . ?; 
        msubleq inc sp;
        msubleq Z Z (-1);
    ```
    还是以 tismain 为例。`msubleq ?+2 0 _main;` 是函数调用，要跳转到 `_main` 函数中。
    之后可以看到 `. ?;` 这句话，这里的 `?` 就代表了下一条指令 `msubleq inc sp` 的起始地址。
    注意到在之前还有一条指令叫做 `msubleq ?+8;` ，这里的 `?+8` 代表的是 `msubleq ?+2 0 _main`; 中 `0` 的地址 所以 `msubleq ?+8` 指令的含义是将 0 的地址清零（因为这个地方可能因为后续的执行而不再是 0）
- `id` 是以 `_` 以及数字，大小写字母，且不会以数字开头。

### Input Format

读入 `*.dmp` 文件格式的内容

### Output Format

输出 `*.tis` 文件格式的内容

### Samples

#### Sample 1

**Input:**
```
top:
    msubleq top top tismain;
_main:
    msubleq dec sp;
    msubleq ?+15;
    msubleq sp ?+10;
    msubleq ?+8;
    msubleq sp ?+3;
    msubleq 0;
    msubleq ?+8;
    msubleq sp ?+3;
    msubleq bp 0;
    msubleq bp;
    msubleq sp bp;

    msubleq sp;
    msubleq bp sp;
    msubleq ?+11;
    msubleq sp ?+6;
    msubleq bp;
    msubleq 0 bp;
    msubleq inc sp;
    msubleq ?+11;
    msubleq sp ?+6;
    msubleq ?+9;
    msubleq 0 ?+4;
    msubleq Z Z 0;

tismain:
    msubleq dec sp;
    msubleq ?+15;
    msubleq sp ?+10;
    msubleq ?+8;
    msubleq sp ?+3;
    msubleq 0;
    msubleq ?+8;
    msubleq sp ?+3;
    msubleq ?+2 0 _main;
    . ?;
    msubleq inc sp;
    msubleq Z Z (-1);

. inc:-1 Z:0 dec:1 ax:0 bp:0 sp:-sp;
```

**Output:**
```
0 0 0 96 
0 143 146 8 
0 25 25 12 
0 146 25 16 
0 26 26 20 
0 146 26 24 
0 0 0 28 
0 38 38 32 
0 146 38 36 
0 145 0 40 
0 145 145 44 
0 146 145 48 
0 146 146 52 
0 145 146 56 
0 69 69 60 
0 146 69 64 
0 145 145 68 
0 0 145 72 
0 141 146 76 
0 89 89 80 
0 146 89 84 
0 95 95 88 
0 0 95 92 
0 142 142 0 
0 143 146 100 
0 117 117 104 
0 146 117 108 
0 118 118 112 
0 146 118 116 
0 0 0 120 
0 130 130 124 
0 146 130 128 
0 132 0 4 
133 
0 141 146 137 
0 142 142 -1 
-1 0 1 0 0 -146
```

### Data Constraints

时间限制：2000 ms
空间限制：512  mb

所有测试数据均已经下发

### 评分标准

每个测试点满分为 `200` 分

任务一：
如果你可以将该测试点的 `*.dmp` 文件成功编译成一个可以执行的 `*.tis` 文件，并得到正确的运行结果，你将获得 `100` 分。

任务二：
在完成任务一的基础上，我们会记录你运行时所使用的指令数量。你在这一部分的得分为：

$$
100 \times (1 - \frac{4M + L + 0R}{Q})
$$

- `Q` 是不加任何优化直接运行时的总花费。
- `M` 是 `msubleq` 指令执行的次数，执行一次花费为 4。
- `L` 是 `ldorst` 指令执行的次数，执行一次花费为 1。
- `R` 是 `rsubleq` 指令执行的次数，执行不需要花费。

当然，如果你的程序运行得太久，我们可能会提前终止你的程序。

## Per-Testcase Resource Limits

- **Time Limit (per test case)**: 2000 ms
- **Memory Limit (per test case)**: 512 MiB
- **Disk Usage**: No disk usage is permitted.

## Test Data

The test data for this problem is located at `/workspace/data/051/data_test/`.

## Submission Requirements

### OJ Git Repository Compilation Process

For Git compilation, we will first clone the repository using a command similar to:
```bash
git clone <repo_url> . --depth 1 --recurse-submodules --shallow-submodules --no-local
```

Then we check if there is a `CMakeLists.txt` file. If it exists, we run (if not, a warning message will be displayed):
```bash
cmake .
```

Finally, we check if there is any of `GNUmakefile`/`makefile`/`Makefile` (if cmake was run previously, this will be the generated Makefile). If it exists, we run (if not, a warning message will be displayed):
```bash
make
```

After this process is complete, we will use the `code` file in the project root directory as the compilation result.

A `CMakeLists.txt` file is provided in the project. You can use or modify it as needed. The local environment has gcc-13 and g++-13 available.

### Git Configuration Requirements

**IMPORTANT**: You must create a `.gitignore` file in your project root directory to avoid OJ evaluation conflicts.

The `.gitignore` file should include at least the following entries:

```gitignore
CMakeFiles/
CMakeCache.txt
```

### Submission Guidelines

- The submitted code must be able to compile successfully through the above compilation process
- The compiled executable file name must be `code`
- The program needs to be able to read data from standard input and write results to standard output
- Please ensure the code runs correctly within the given time and space limits
- **You must use C++ or C language** to implement this assignment

### Evaluation Notes

- The evaluation system will test your program using the provided test data
- The program output must exactly match the expected output (including format)
- Exceeding time or memory limits will be judged as the corresponding error type
- Please pay attention to the overall time performance of your code and the time complexity of each part of your algorithm.

### Academic Integrity

If any violations are found during evaluation or code review (including but not limited to using unconventional methods to pass test cases), your final score may be significantly reduced or become **0 points**.
