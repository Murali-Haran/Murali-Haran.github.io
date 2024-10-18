# Create x as a big number plus a tiny number
x = 100000 + 0.000001
x
# The print command allows you to specify the number of
# digits to show when printing to the console.
print(x, digits = 10)
# [1] 1e+05

# Try more digits
print(x, digits = 20)
# [1] 100000.00000099999306
# Notice that it is not 100000.000001
# That's because in the conversion from decimal to binary
# there is some rounding error

# Now, divide x by 50,000 and we get a number that appears to be 2
y = x/50000
y
# [1] 2

# Check to see if it is 2 with the ==== operator
y == 2
# [1] FALSE
# We get FALSE not TRUE???
# This is because although it prints to the screen as 2,
# It in fact is not true.
# It's typically not a good practice to use == when checking 
# the value of a numeric variable.
# It's better to determine a tolerance and see if it falls within
# an acceptable range. Or to use the function all.equal()


# Let's explore a bit more by making another number that is
# not 2. It's 2 plus a tiny bit.
yPTinyBit = 2.00000000000000000000001
yPTinyBit == 2
# [1] TRUE

# Now why didn't we get FALSE this time?
# Because the number is rounded - the computer can't  store
# arbitrarily small values. It is a finite state machine.
# Let's print this 
print(yPTinyBit, digits = 22)
# [1] 2
# Look - it is 2 to 22 digits.
# let's look at it for 23 digits
print(yPTinyBit, digits = 23)
# Error in print.default(yPTinyBit, digits = 23) : 
#  invalid 'digits' argument
# The print function doesn't go that far due to limitations
# in the size of a number that can be stored.

# We can use all.equal if we want to make a comparison 
# within a certain tolerance
all.equal(y, 2)
#[1] TRUE
all.equal(yPTinyBit, 2)
#[1] TRUE
all.equal(y, yPTinyBit)
#[1] TRUE

# Default Tolerance used in all.equal
.Machine$double.eps ^ 0.5
#[1] 1.490116e-08
# .Machine$double.eps is the smallest value 
#  such that 1 + eps != 1

print(1 + .Machine$double.eps, digits = 22)
#[1] 1.000000000000000222045
print(1 + .Machine$double.eps^2, digits = 22)
#[1] 1
print(1 + .Machine$double.eps^0.5, digits = 22)
#[1] 1.000000014901161193848
# May also want to compute relative difference.
# In this case supply a value for the scale parameter, 
# e.g. min(abs(A), abs(B))



# Let's continue the exploration this time with computations
# and small and big numbers.
vv = c(100000, rep(0.1, 200))
# This vector has 201 elements, the first is 100000 and the 
# rest are 0.1.
# Let's add them up. We should get 100,020 - Right?

vvSum = sum(vv)
vvSum
# [1] 100020
# Looks good, but let's check more digits
print(vvSum, digits = 22)
# [1] 100020
# Still looking good.


# Let's write our own loop to do the summation. 
# It's pretty simple.
ww = 0
for (i in vv) {ww = ww + i}
ww
# [1] 100020
# Looks good, but let's check more digits.
print(ww, digits = 22)
# [1] 100020.0000000011641532
# Well - we get a different answer than when we 
# call the sum() function?  

# Let's try it again, but this time when we sum in the
# reverse order.  The rev() function reverses the order
# of the elements in its argument
vvRev = rev(vv)
vvRev[1]
# [1] 0.1
vvRev[201]
# [1] 1e+05

wwRev = 0
for (i in vvRev) {wwRev = wwRev + i}
wwRev
# [1] 100020
# Again it looks like it is 100020, but is it really?
print(wwRev, digits = 22)
# [1] 100020
# Whoa - it is the same as sum(vv), but not the same
# as the first for loop.
# So the order that we add numbers together matters.

# This is because of how the numbers are represented in the 
# computer.  When we add a bunch of small numbers together
# it is able to maintain the accuracy.
# When we add a small number to a big number and do that 
# over and over and over then the rounding matters.
# That is, there's very little difference between 100,000 and 
# 100,000.1 but when you add 0.1 to a big number hundreds 
# or thousands of times then those little numbers add up - 
# or in our case the errors add up!
