library("Rcpp")
cppFunction('int add(int x, int y, int z){
            int sum = x + y + z;
            return sum;
}')

add
add(1,2,3)

add(-2,0,2)
add(-2.1,1,1)
add(-2,)
add(-2)
add(1,-2,)
