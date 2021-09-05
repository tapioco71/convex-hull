# convex-hull
### Angelo Rossi <angelo.rossi.homelab@gmail.com>

A simple project to compute the convex hull of a set of points in xy plane.

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


The algorithm is a modified Graham's scan from 
https://cp-algorithms.com/geometry/grahams-scan-convex-hull.html
To use the program just type:

CL-USER> (cvh:main #(0d0 1d0 1d0 0.5d0  0d0)
                   #(0d0 0d0 1d0 0.75d0 1d0)
                   :point-positions-list '(1 2 3 4 5)
                   :verbose t)
                   
The verbosity could be omitted for zero messages from the program. first input
is the x array for the points, the second is the y coordinates array for the same
points. The third is a list containing the positions of the points in the two arrays
x and y to use in the computation. If this parameter is omitted all the points are
used for the computation. The result is a list of point of the type:

((:position 1 :coordinates #(0d0 0d0))
 (:position 2 :coordinates #(1d0 0d0))
 (:position 3 :coordinates #(1d0 1d0))
 (:position 5 :coordinates #(0d0 1d0)))
