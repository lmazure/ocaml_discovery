# Some very little theory

If ((a,b,c) is a Pythogorian triplet, then we have a magic square of squares:
<table>
<tr><td>c<sup>2</sup></td><td>(a+b)<sup>2</sup></td><td>(b-a)<sup>2</sup></td></tr>
<tr><td>(b-a)<sup>2</sup></td><td>c<sup>2</sup></td><td>(a+b)<sup>2</sup></td></tr>
<tr><td>(a+b)<sup>2</sup></td><td>(b-a)<sup>2</sup></td><td>c<sup>2</sup></td></tr>
</table>

# First try

##
Looking on trying [-1, 0, 1] on all dimensions and picking the best option among the 3^9 ones.  
Then repeating this until no better solution is found.  
If the found optimum has repeated numbers, it is rejected.  
The score is defined by the difference between the largest sum and the smallest sum.

## Best finding for 2-digits numbers
<table>
<tr><td>21<sup>2</sup></td><td>20<sup>2</sup></td><td>29<sup>2</sup></td></tr>
<tr><td>31<sup>2</sup></td><td>24<sup>2</sup></td><td>13<sup>2</sup></td></tr>
<tr><td>17<sup>2</sup></td><td>27<sup>2</sup></td><td>26<sup>2</sup></td></tr>
</table>
line 1:   1682<br/>
line 2:   1706<br/>
line 3:   1694<br/>
column 1: 1691<br/>
column 2: 1705<br/>
column 3: 1686<br/>
diag 1:   1693<br/>
diag 2:   1706<br/>
error: 1706 - 1682 = 24<br/>

## Best finding for 3-digits numbers
<table>
<tr><td>514<sup>2</sup></td><td>357<sup>2</sup></td><td>542<sup>2</sup></td></tr>
<tr><td>508<sup>2</sup></td><td>478<sup>2</sup></td><td>446<sup>2</sup></td></tr>
<tr><td>404<sup>2</sup></td><td>574<sup>2</sup></td><td>439<sup>2</sup></td></tr>
</table>
line 1:   685409<br/>
line 2:   685464<br/>
line 3:   685413<br/>
column 1: 685476<br/>
column 2: 685409<br/>
column 3: 685401<br/>
diag 1:   685401<br/>
diag 2:   685464<br/>
error: 685476 - 685401 = 75<br/>

## Best finding for 4-digits numbers
<table>
<tr><td>3621<sup>2</sup></td><td>3440<sup>2</sup></td><td>3619<sup>2</sup></td></tr>
<tr><td>3559<sup>2</sup></td><td>3561<sup>2</sup></td><td>3563<sup>2</sup></td></tr>
<tr><td>3502<sup>2</sup></td><td>3678<sup>2</sup></td><td>3500<sup>2</sup></td></tr>
</table>
line 1:   38042402<br/>
line 2:   38042171<br/>
line 3:   38041688<br/>
column 1: 38042126<br/>
column 2: 38042005<br/>
column 3: 38042130<br/>
diag 1:   38042362<br/>
diag 2:   38041886<br/>
error: 38042402 - 38041688 = 714<br/>
