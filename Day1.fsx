let day1Input = """1211
1698
1787
1947
1888
444
1819
1890
1940
1884
1917
1814
1724
1561
1823
1266
1982
1826
1871
1692
1665
1081
1847
640
1861
1632
1916
1921
1450
1806
1950
1969
1757
1766
1799
422
1865
1934
1954
1640
1743
1812
1745
1574
1904
1510
1491
1977
1727
1979
1842
1784
1655
1991
1296
1849
1863
1886
1696
1716
1679
1848
1540
1780
1926
1986
1898
1448
315
1568
1869
1875
2010
1268
1892
1248
1746
1987
1963
20
1575
1827
1653
1851
1365
1599
1688
1943
1677
1320
154
1490
1737
1573
1908
1667
1151
1761
1587
1924
1941
1731
1669
1857
1723
1880
1970
1791
1928
1942
1816
1989
1832
1911
1711
1817
1893
896
1998
1720
317
1964
1379
1750
1971
1322
1992
1347
1608
1373
1668
1252
373
1968
1754
1709
1988
1946
1537
1758
1830
624
1694
1914
1867
1145
1973
1769
1773
1424
1777
1659
1789
1907
1201
1967
1682
1952
1978
1937
1974
1488
1896
1657
1420
1935
1778
1822
1703
2003
119
1149
1732
1878
1938
1918
1797
1836
1741
1579
1589
1999
1772
1853
1793
1768
1759
1216
1765
1944
1735
1580
1756
1308
1786
1962
1981
1156
1948
1894"""

type SumExpression = SumExpression of int * int * int
type TripleSumExpression = TripleSumExpression of int * int * int * int

let parseInput (x: string) =
    x.Split([| '\n' |]) |> Set.ofArray |> Set.map int

let generateSums ns =
    seq {
        for a in ns do
            yield!
                seq {
                    for b in ns do
                        yield SumExpression(a, b, a + b)
                }
    }

let generateTripleSums ns =
    seq {
        for a in ns do
            yield!
                seq {
                    for b in ns do
                        yield!
                            seq {
                                for c in ns do
                                    yield TripleSumExpression(a, b, c, a + b + c)
                            }
                }
    }

let day1Part1Solution =
    parseInput day1Input
    |> generateSums
    |> Seq.tryFind (fun exp ->
        match exp with
        | SumExpression (_, _, res) -> res = 2020)

let day1Part2Solution =
    parseInput day1Input
    |> generateTripleSums
    |> Seq.tryFind (fun exp ->
        match exp with
        | TripleSumExpression (_, _, _, res) -> res = 2020)

match day1Part1Solution with
| Some (SumExpression (a, b, sum)) -> printfn "%i + %i = %i, %i * %i = %i" a b sum a b (a * b)
| None -> do printfn "Part1: Not found."

match day1Part2Solution with
| Some (TripleSumExpression (a, b, c, sum)) ->
    printfn "%i + %i + %i = %i, %i * %i * %i = %i" a b c sum a b c (a * b * c)
| None -> do printfn "Part2: Not found."
