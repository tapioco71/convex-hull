# convex-hull
### Angelo Rossi <angelo.rossi.homelab@gmail.com>

A simple project to compute the convex hull of a set of points in xy
plane.

## License

BSD

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

## Description

The algorithm is a modified Graham's scan from
https://cp-algorithms.com/geometry/grahams-scan-convex-hull.html
To use the program just type:

```
CL-USER> (cvh::main (mapcar #'(lambda (x)
                                (grid:make-foreign-array 'double-float
                                                         :initial-contents x))
                            '((0d0 0d0)
                              (2d0 0d0)
                              (0d0 2d0)
                              (2d0 2d0)
                              (0d0 4d0)
                              (2d0 4d0)
                              (0d0 6d0)
                              (2d0 6d0)
                              (0d0 8d0)
                              (2d0 8d0)
                              (4d0 6d0)
                              (4d0 8d0)
                              (6d0 6d0)
                              (6d0 8d0)
                              (8d0 6d0)
                              (8d0 8d0))))
((#m(0.000000000000000d0 0.000000000000000d0)
  #m(2.000000000000000d0 0.000000000000000d0))
 (#m(2.000000000000000d0 0.000000000000000d0)
  #m(8.000000000000000d0 6.000000000000000d0))
 (#m(8.000000000000000d0 6.000000000000000d0)
  #m(8.000000000000000d0 8.000000000000000d0))
 (#m(8.000000000000000d0 8.000000000000000d0)
  #m(0.000000000000000d0 8.000000000000000d0))
 (#m(0.000000000000000d0 8.000000000000000d0)
  #m(0.000000000000000d0 0.000000000000000d0)))
CL-USER>
```

The verbosity could be omitted for zero messages from the program. The input is
a list of grid:foreign-array (dimension 2) for the vertex points.
The second input is the maximum number of iterations admitted for computations.
The third is the verbose flag and could be nil or t. The function returns
the couple of vertices identifying a polygon edge.
