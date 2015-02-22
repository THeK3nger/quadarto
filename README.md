# Quadarto

![Example](./images/example.png)

**Quadarto** is an Haskell script which performs image tessellation using a
[QuadTree][1] pattern. The algorithm iterates over the image merging similar
square regions of the images on the basis of their level of details.

The algorithm is developed in order as a small toy software for Haskell learning
purpose and I'll try to be idiomatic as much as possible.

## Development

There is still a lot of work to do on the algorithm. The software works but it
is still heavily non optimized and non idiomatic. I think can be interesting to
follow the various commits I'll do to see how the software evolve towards its
final idiomatic Haskell version. :)


  [1]: http://en.wikipedia.org/wiki/Quadtree
