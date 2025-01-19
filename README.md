# Autograd.hs
Automatic differentiation engine in Haskell.

After cloning the project, to run the tests simply execute
```
cabal build
cabal run
```

To implement your own custom operators, you need to create an instance of the [`Operator`](./Autograd.hs) typeclass. However, if you're just working with scalar-valued functions ($f : \mathbb{R}^d \to \mathbb{R}$), extending [`ScalarOp`](./ScalarOps.hs) should be enough.

References: 
- https://pytorch.org/tutorials/beginner/introyt/autogradyt_tutorial.html
- https://github.com/karpathy/micrograd

Since in Haskell there is no such thing as a "*reference to an object*", my solution to accumulate the gradients when a node is a node to more than one parent in the computational graph is to traverse the graph and fill a map of $\text{Id} \rightarrow \text{Gradient}$, one drawback of this method is that each node identifier must be unique. I.e, we do not perform a topological sort such as in Micrograd.

*I started this project mostly to learn the basics of programming in Haskell. This library is a toy project and has no pretensions of performance or correctness.*
