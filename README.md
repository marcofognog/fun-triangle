# Fun Triangle

Given a triangle of numbers, find the maximum total from top to bottom. Each line has one number more the previous.

Example:

6

3 5

9 7 1

4 6 8 4

In this triangle the maximum total is 6 + 5 + 7 + 8 = 26

An element can only be summed with one of the two nearest elements in the next row. So the element 3 in row 2 can be summed with 9 and 7, but not with 1.

Your code will should receive an (multidimensional) array as input, the triangle from above would be:

example = [[6],[3,5],[9,7,1],[4,6,8,4]]
