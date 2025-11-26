module preDefEnergyGrids

  use numPrecision

  implicit none

  real(defReal),dimension(70),parameter :: wims69 = [ &
    1.0000000000E+01_defReal,&
    6.0655000000E+00_defReal,&
    3.6790000000E+00_defReal,&
    2.2310000000E+00_defReal,&
    1.3530000000E+00_defReal,&
    8.2100000000E-01_defReal,&
    5.0000000000E-01_defReal,&
    3.0250000000E-01_defReal,&
    1.8300000000E-01_defReal,&
    1.1100000000E-01_defReal,&
    6.7340000000E-02_defReal,&
    4.0850000000E-02_defReal,&
    2.4780000000E-02_defReal,&
    1.5030000000E-02_defReal,&
    9.1180000000E-03_defReal,&
    5.5300000000E-03_defReal,&
    3.5191000000E-03_defReal,&
    2.2394500000E-03_defReal,&
    1.4251000000E-03_defReal,&
    9.0689900000E-04_defReal,&
    3.6726300000E-04_defReal,&
    1.4872900000E-04_defReal,&
    7.5501400000E-05_defReal,&
    4.8052000000E-05_defReal,&
    2.7700000000E-05_defReal,&
    1.5968000000E-05_defReal,&
    9.8770000000E-06_defReal,&
    4.0000000000E-06_defReal,&
    3.3000000000E-06_defReal,&
    2.6000000000E-06_defReal,&
    2.1000000000E-06_defReal,&
    1.5000000000E-06_defReal,&
    1.3000000000E-06_defReal,&
    1.1500000000E-06_defReal,&
    1.1230000000E-06_defReal,&
    1.0970000000E-06_defReal,&
    1.0710000000E-06_defReal,&
    1.0450000000E-06_defReal,&
    1.0200000000E-06_defReal,&
    9.9600000000E-07_defReal,&
    9.7200000000E-07_defReal,&
    9.5000000000E-07_defReal,&
    9.1000000000E-07_defReal,&
    8.5000000000E-07_defReal,&
    7.8000000000E-07_defReal,&
    6.2500000000E-07_defReal,&
    5.0000000000E-07_defReal,&
    4.0000000000E-07_defReal,&
    3.5000000000E-07_defReal,&
    3.2000000000E-07_defReal,&
    3.0000000000E-07_defReal,&
    2.8000000000E-07_defReal,&
    2.5000000000E-07_defReal,&
    2.2000000000E-07_defReal,&
    1.8000000000E-07_defReal,&
    1.4000000000E-07_defReal,&
    1.0000000000E-07_defReal,&
    8.0000000000E-08_defReal,&
    6.7000000000E-08_defReal,&
    5.8000000000E-08_defReal,&
    5.0000000000E-08_defReal,&
    4.2000000000E-08_defReal,&
    3.5000000000E-08_defReal,&
    3.0000000000E-08_defReal,&
    2.5000000000E-08_defReal,&
    2.0000000000E-08_defReal,&
    1.5000000000E-08_defReal,&
    1.0000000000E-08_defReal,&
    5.0000000000E-09_defReal,&
    1.0000000000E-11_defReal]

real(defReal),dimension(173),parameter :: wims172 = [ &
    1.9640300000E+01_defReal,&
    1.7332500000E+01_defReal,&
    1.4918200000E+01_defReal,&
    1.3840300000E+01_defReal,&
    1.1618300000E+01_defReal,&
    1.0000000000E+01_defReal,&
    8.1873100000E+00_defReal,&
    6.7032000000E+00_defReal,&
    6.0653100000E+00_defReal,&
    5.4881200000E+00_defReal,&
    4.4932900000E+00_defReal,&
    3.6787900000E+00_defReal,&
    3.0119400000E+00_defReal,&
    2.4659700000E+00_defReal,&
    2.2313000000E+00_defReal,&
    2.0189700000E+00_defReal,&
    1.6529900000E+00_defReal,&
    1.3533500000E+00_defReal,&
    1.2245600000E+00_defReal,&
    1.1080300000E+00_defReal,&
    1.0025900000E+00_defReal,&
    9.0718000000E-01_defReal,&
    8.2085000000E-01_defReal,&
    6.0810100000E-01_defReal,&
    5.5023200000E-01_defReal,&
    4.9787100000E-01_defReal,&
    4.5049200000E-01_defReal,&
    4.0762200000E-01_defReal,&
    3.0197400000E-01_defReal,&
    2.7323700000E-01_defReal,&
    2.4723500000E-01_defReal,&
    1.8315600000E-01_defReal,&
    1.2277300000E-01_defReal,&
    1.1109000000E-01_defReal,&
    8.2297500000E-02_defReal,&
    6.7379500000E-02_defReal,&
    5.5165600000E-02_defReal,&
    4.0867700000E-02_defReal,&
    3.6978600000E-02_defReal,&
    2.9283000000E-02_defReal,&
    2.7394400000E-02_defReal,&
    2.4787500000E-02_defReal,&
    1.6615600000E-02_defReal,&
    1.5034400000E-02_defReal,&
    1.1137800000E-02_defReal,&
    9.1188200000E-03_defReal,&
    7.4658600000E-03_defReal,&
    5.5308500000E-03_defReal,&
    5.0045000000E-03_defReal,&
    3.5266200000E-03_defReal,&
    3.3546300000E-03_defReal,&
    2.2486700000E-03_defReal,&
    2.0346800000E-03_defReal,&
    1.5073300000E-03_defReal,&
    1.4338200000E-03_defReal,&
    1.2341000000E-03_defReal,&
    1.0103900000E-03_defReal,&
    9.1424200000E-04_defReal,&
    7.4851800000E-04_defReal,&
    6.7728700000E-04_defReal,&
    4.5399900000E-04_defReal,&
    3.7170300000E-04_defReal,&
    3.0432500000E-04_defReal,&
    2.0399500000E-04_defReal,&
    1.4862500000E-04_defReal,&
    1.3674200000E-04_defReal,&
    9.1660900000E-05_defReal,&
    7.5673600000E-05_defReal,&
    6.7904000000E-05_defReal,&
    5.5595100000E-05_defReal,&
    5.1578000000E-05_defReal,&
    4.8251600000E-05_defReal,&
    4.5517400000E-05_defReal,&
    4.0169000000E-05_defReal,&
    3.7266500000E-05_defReal,&
    3.3720100000E-05_defReal,&
    3.0511300000E-05_defReal,&
    2.7607700000E-05_defReal,&
    2.4980500000E-05_defReal,&
    2.2603300000E-05_defReal,&
    1.9454800000E-05_defReal,&
    1.5928300000E-05_defReal,&
    1.3709600000E-05_defReal,&
    1.1224500000E-05_defReal,&
    9.9055500000E-06_defReal,&
    9.1898100000E-06_defReal,&
    8.3152900000E-06_defReal,&
    7.5239800000E-06_defReal,&
    6.1601200000E-06_defReal,&
    5.3464300000E-06_defReal,&
    5.0434800000E-06_defReal,&
    4.1292500000E-06_defReal,&
    4.0000000000E-06_defReal,&
    3.3807500000E-06_defReal,&
    3.3000000000E-06_defReal,&
    2.7679200000E-06_defReal,&
    2.7200000000E-06_defReal,&
    2.6000000000E-06_defReal,&
    2.5500000000E-06_defReal,&
    2.3600000000E-06_defReal,&
    2.1300000000E-06_defReal,&
    2.1000000000E-06_defReal,&
    2.0200000000E-06_defReal,&
    1.9300000000E-06_defReal,&
    1.8400000000E-06_defReal,&
    1.7550000000E-06_defReal,&
    1.6700000000E-06_defReal,&
    1.5900000000E-06_defReal,&
    1.5000000000E-06_defReal,&
    1.4750000000E-06_defReal,&
    1.4449800000E-06_defReal,&
    1.3700000000E-06_defReal,&
    1.3375000000E-06_defReal,&
    1.3000000000E-06_defReal,&
    1.2350000000E-06_defReal,&
    1.1700000000E-06_defReal,&
    1.1500000000E-06_defReal,&
    1.1253500000E-06_defReal,&
    1.1100000000E-06_defReal,&
    1.0970000000E-06_defReal,&
    1.0710000000E-06_defReal,&
    1.0450000000E-06_defReal,&
    1.0350000000E-06_defReal,&
    1.0200000000E-06_defReal,&
    9.9600000000E-07_defReal,&
    9.8600000000E-07_defReal,&
    9.7200000000E-07_defReal,&
    9.5000000000E-07_defReal,&
    9.3000000000E-07_defReal,&
    9.1000000000E-07_defReal,&
    8.6000000000E-07_defReal,&
    8.5000000000E-07_defReal,&
    7.9000000000E-07_defReal,&
    7.8000000000E-07_defReal,&
    7.0500000000E-07_defReal,&
    6.2500000000E-07_defReal,&
    5.4000000000E-07_defReal,&
    5.0000000000E-07_defReal,&
    4.8500000000E-07_defReal,&
    4.3300000000E-07_defReal,&
    4.0000000000E-07_defReal,&
    3.9100000000E-07_defReal,&
    3.5000000000E-07_defReal,&
    3.2000000000E-07_defReal,&
    3.1450000000E-07_defReal,&
    3.0000000000E-07_defReal,&
    2.8000000000E-07_defReal,&
    2.4800000000E-07_defReal,&
    2.2000000000E-07_defReal,&
    1.8900000000E-07_defReal,&
    1.8000000000E-07_defReal,&
    1.6000000000E-07_defReal,&
    1.4000000000E-07_defReal,&
    1.3400000000E-07_defReal,&
    1.1500000000E-07_defReal,&
    1.0000000000E-07_defReal,&
    9.5000000000E-08_defReal,&
    8.0000000000E-08_defReal,&
    7.7000000000E-08_defReal,&
    6.7000000000E-08_defReal,&
    5.8000000000E-08_defReal,&
    5.0000000000E-08_defReal,&
    4.2000000000E-08_defReal,&
    3.5000000000E-08_defReal,&
    3.0000000000E-08_defReal,&
    2.5000000000E-08_defReal,&
    2.0000000000E-08_defReal,&
    1.5000000000E-08_defReal,&
    1.0000000000E-08_defReal,&
    6.9000000000E-09_defReal,&
    5.0000000000E-09_defReal,&
    3.0000000000E-09_defReal,&
    1.0000000000E-11_defReal]

real(defReal),dimension(41),parameter :: casmo40 = [ &
    1.0000000000E+01_defReal,&
    6.0655000000E+00_defReal,&
    3.6790000000E+00_defReal,&
    2.2310000000E+00_defReal,&
    1.3530000000E+00_defReal,&
    8.2100000000E-01_defReal,&
    5.0000000000E-01_defReal,&
    1.1100000000E-01_defReal,&
    9.1180000000E-03_defReal,&
    5.5300000000E-03_defReal,&
    1.4872800000E-04_defReal,&
    4.8052000000E-05_defReal,&
    2.7700000000E-05_defReal,&
    1.5968000000E-05_defReal,&
    9.8770000000E-06_defReal,&
    4.0000000000E-06_defReal,&
    3.3000000000E-06_defReal,&
    2.6000000000E-06_defReal,&
    2.1000000000E-06_defReal,&
    1.8550000000E-06_defReal,&
    1.5000000000E-06_defReal,&
    1.3000000000E-06_defReal,&
    1.1500000000E-06_defReal,&
    1.0970000000E-06_defReal,&
    1.0200000000E-06_defReal,&
    9.7200000000E-07_defReal,&
    9.5000000000E-07_defReal,&
    8.5000000000E-07_defReal,&
    6.2500000000E-07_defReal,&
    3.5000000000E-07_defReal,&
    2.8000000000E-07_defReal,&
    2.2000000000E-07_defReal,&
    1.8000000000E-07_defReal,&
    1.4000000000E-07_defReal,&
    1.0000000000E-07_defReal,&
    8.0000000000E-08_defReal,&
    5.8000000000E-08_defReal,&
    4.2000000000E-08_defReal,&
    3.0000000000E-08_defReal,&
    1.5000000000E-08_defReal,&
    1.0000000000E-11_defReal]

real(defReal),dimension(24),parameter :: casmo23 = [ &
    1.0000000E+01_defReal,&
    6.0655000E+00_defReal,&
    3.6790000E+00_defReal,&
    2.2310000E+00_defReal,&
    1.3530000E+00_defReal,&
    8.2100000E-01_defReal,&
    5.0000000E-01_defReal,&
    1.1100000E-01_defReal,&
    9.1180000E-03_defReal,&
    5.5300000E-03_defReal,&
    1.4872800E-04_defReal,&
    1.5968000E-05_defReal,&
    9.8770000E-06_defReal,&
    4.0000000E-06_defReal,&
    1.8550000E-06_defReal,&
    1.0970000E-06_defReal,&
    1.0200000E-06_defReal,&
    6.2500000E-07_defReal,&
    3.5000000E-07_defReal,&
    2.8000000E-07_defReal,&
    1.4000000E-07_defReal,&
    5.8000000E-08_defReal,&
    3.0000000E-08_defReal,&
    1.0000000E-11_defReal]

real(defReal),dimension(13),parameter :: casmo12 = [ &
    1.00000E+01_defReal,&
    2.23100E+00_defReal,&
    8.21000E-01_defReal,&
    5.53000E-03_defReal,&
    4.80520E-05_defReal,&
    4.00000E-06_defReal,&
    6.25000E-07_defReal,&
    3.50000E-07_defReal,&
    2.80000E-07_defReal,&
    1.40000E-07_defReal,&
    5.80000E-08_defReal,&
    3.00000E-08_defReal,&
    1.00000E-11_defReal]

real(defReal),dimension(8),parameter :: casmo7 = [ &
    1.00000E+01_defReal,&
    8.21000E-01_defReal,&
    5.53000E-03_defReal,&
    4.00000E-06_defReal,&
    6.25000E-07_defReal,&
    1.40000E-07_defReal,&
    5.80000E-08_defReal,&
    1.00000E-11_defReal]

real(defReal),dimension(4),parameter :: ecco3 = [ &
    1.964033E+01_defReal,&
    4.978707E-01_defReal,&
    7.485183E-04_defReal,&
    1.000010E-11_defReal]

real(defReal),dimension(7),parameter :: ecco6 = [ &
    1.964033E+01_defReal,&
    2.231302E+00_defReal,&
    4.978707E-01_defReal,&
    2.478752E-02_defReal,&
    5.530844E-03_defReal,&
    7.485183E-04_defReal,&
    1.000010E-11_defReal]

real(defReal),dimension(9),parameter :: ecco9 = [ &
    1.964033E+01_defReal,&
    2.231302E+00_defReal,&
    4.978707E-01_defReal,&
    1.110900E-01_defReal,&
    2.478752E-02_defReal,&
    5.530844E-03_defReal,&
    2.034684E-03_defReal,&
    7.485183E-04_defReal,&
    !5.400000E-07_defReal,&
    1.000010E-11_defReal]

real(defReal),dimension(16),parameter :: ecco16 = [ &
    1.964033E+01_defReal,&
    6.065307E+00_defReal,&
    2.231302E+00_defReal,&
    8.208500E-01_defReal,&
    4.978707E-01_defReal,&
    3.019738E-01_defReal,&
    1.110900E-01_defReal,&
    6.737947E-02_defReal,&
    2.478752E-02_defReal,&
    1.503439E-02_defReal,&
    5.530844E-03_defReal,&
    3.354626E-03_defReal,&
    2.034684E-03_defReal,&
    1.234098E-03_defReal,&
    7.485183E-04_defReal,&
    !5.400000E-07_defReal,&
    1.000010E-11_defReal]

real(defReal),dimension(22),parameter :: ecco33 = [ &
    1.964033E+01_defReal,&
    1.000000E+01_defReal,&
    6.065307E+00_defReal,&
    3.678794E+00_defReal,&
    2.231302E+00_defReal,&
    1.353353E+00_defReal,&
    8.208500E-01_defReal,&
    4.978707E-01_defReal,&
    3.019738E-01_defReal,&
    1.831564E-01_defReal,&
    1.110900E-01_defReal,&
    6.737947E-02_defReal,&
    4.086771E-02_defReal,&
    2.478752E-02_defReal,&
    1.503439E-02_defReal,&
    9.118820E-03_defReal,&
    5.530844E-03_defReal,&
    3.354626E-03_defReal,&
    2.034684E-03_defReal,&
    1.234098E-03_defReal,&
    7.485183E-04_defReal,&
    !4.539993E-04_defReal,&
    !3.043248E-04_defReal,&
    !1.486254E-04_defReal,&
    !9.166088E-05_defReal,&
    !6.790405E-05_defReal,&
    !4.016900E-05_defReal,&
    !2.260329E-05_defReal,&
    !1.370959E-05_defReal,&
    !8.315287E-06_defReal,&
    !4.000000E-06_defReal,&
    !5.400000E-07_defReal,&
    !1.000000E-07_defReal,&
    1.000010E-11_defReal]

real(defReal),dimension(331),parameter :: ecco330 = [ &
    19.6403300000000_defReal, 18.3583608705849_defReal,&
    17.1600687898128_defReal, 16.0399919658909_defReal,&
    14.9930251106324_defReal, 14.0143961696535_defReal,&
    13.0996445714427_defReal, 12.2446008961633_defReal,&
    11.4453678715202_defReal, 10.6983026090685_defReal,&
    9.99999999999999_defReal, 9.51229430819030_defReal,&
    9.04837430056297_defReal, 8.60707993576206_defReal,&
    8.18730774830884_defReal, 7.78800808936406_defReal,&
    7.40818250205978_defReal, 7.04688122483783_defReal,&
    6.70320081655180_defReal, 6.37628189739423_defReal,&
    6.06530700000000_defReal, 5.76949838413794_defReal,&
    5.48811652972724_defReal, 5.22045783506458_defReal,&
    4.96585301351858_defReal, 4.72366541996340_defReal,&
    4.49328945883318_defReal, 4.27414906981659_defReal,&
    4.06569628740499_defReal, 3.86740987069224_defReal,&
    3.67879400000000_defReal, 3.49937720113986_defReal,&
    3.32871065785619_defReal, 3.16636761539059_defReal,&
    3.01194213204801_defReal, 2.86504806413226_defReal,&
    2.72531810038679_defReal, 2.59240284352625_defReal,&
    2.46596993656233_defReal, 2.34570323173913_defReal,&
    2.23130200000000_defReal, 2.12248010572953_defReal,&
    2.01896551843616_defReal, 1.92049939767663_defReal,&
    1.82683552680640_defReal, 1.73773969730970_defReal,&
    1.65298912315602_defReal, 1.57237188371900_defReal,&
    1.49568639386423_defReal, 1.42274089988140_defReal,&
    1.35335300000000_defReal, 1.28734918154864_defReal,&
    1.22456440798074_defReal, 1.16484168459198_defReal,&
    1.10803167340171_defReal, 1.05399231973008_defReal,&
    1.00258849698717_defReal, 0.953691668785982_defReal,&
    0.907179567534408_defReal, 0.862935888702410_defReal,&
    0.820849999999999_defReal, 0.780816674352121_defReal,&
    0.742735796974242_defReal, 0.706512145843578_defReal,&
    0.672055142970048_defReal, 0.639278627904139_defReal,&
    0.608100642290915_defReal, 0.578443224931451_defReal,&
    0.550232216839242_defReal, 0.523397075804121_defReal,&
    0.497870699999999_defReal, 0.473589252516967_defReal,&
    0.450492025539120_defReal, 0.428521263934444_defReal,&
    0.407622029322753_defReal, 0.387742062701043_defReal,&
    0.368831653768687_defReal, 0.350843516625722_defReal,&
    0.333732671533392_defReal, 0.317456332441314_defReal,&
    0.301973799999999_defReal, 0.287246368986515_defReal,&
    0.273237203015417_defReal, 0.259911271899118_defReal,&
    0.247235253891856_defReal, 0.235177452368036_defReal,&
    0.223707716564210_defReal, 0.212797366186516_defReal,&
    0.202419119695055_defReal, 0.192547026085877_defReal,&
    0.183156400000000_defReal, 0.174223761337670_defReal,&
    0.165726772390402_defReal, 0.157644186281275_defReal,&
    0.149955792355277_defReal, 0.142642365642189_defReal,&
    0.135685618784194_defReal, 0.129068156308005_defReal,&
    0.122773431127162_defReal, 0.116785703165741_defReal,&
    0.111090000000000_defReal, 0.105672073476292_defReal,&
    0.100518382507687_defReal, 0.0956160401662645_defReal,&
    0.0909527880273798_defReal, 0.0865169655171746_defReal,&
    0.0822974807550340_defReal, 0.0782837828180728_defReal,&
    0.0744658353582996_defReal, 0.0708340915064878_defReal,&
    0.0673794699999999_defReal, 0.0640933337827445_defReal,&
    0.0609674643535532_defReal, 0.0579940454073009_defReal,&
    0.0551656418446418_defReal, 0.0524751811803798_defReal,&
    0.0499159358585647_defReal, 0.0474815064300900_defReal,&
    0.0451658055507307_defReal, 0.0429630427596050_defReal,&
    0.0408677099999999_defReal, 0.0388745684039739_defReal,&
    0.0369786334638092_defReal, 0.0351751643552901_defReal,&
    0.0334596514669100_defReal, 0.0318278051234953_defReal,&
    0.0302755448597847_defReal, 0.0287989892171420_defReal,&
    0.0273944460378890_defReal, 0.0260584032329898_defReal,&
    0.0247875199999999_defReal, 0.0235786182498010_defReal,&
    0.0224286753402459_defReal, 0.0213348158144254_defReal,&
    0.0202943044531343_defReal, 0.0193045394353970_defReal,&
    0.0183630458325584_defReal, 0.0174674694196714_defReal,&
    0.0166155707887077_defReal, 0.0158052197488690_defReal,&
    0.0150343900000000_defReal, 0.0143011543850065_defReal,&
    0.0136036790813455_defReal, 0.0129402200386185_defReal,&
    0.0123091182647409_defReal, 0.0117087956776006_defReal,&
    0.0111377511590336_defReal, 0.0105945568012487_defReal,&
    0.0100778543363168_defReal, 0.00958635173979612_defReal,&
    0.00911881999999999_defReal, 0.00867408991477755_defReal,&
    0.00825104957106793_defReal, 0.00784864114772855_defReal,&
    0.00746585841416108_defReal, 0.00710174421421103_defReal,&
    0.00675538807277891_defReal, 0.00642592391915841_defReal,&
    0.00611252792140868_defReal, 0.00581441642634543_defReal,&
    0.00553084399999999_defReal, 0.00526110148296710_defReal,&
    0.00500451446724563_defReal, 0.00476044135129401_defReal,&
    0.00452827182485547_defReal, 0.00430742534286364_defReal,&
    0.00409734967377673_defReal, 0.00389751951871027_defReal,&
    0.00370743519791552_defReal, 0.00352662140131919_defReal,&
    0.00335462599999999_defReal, 0.00319101903453779_defReal,&
    0.00303539127127212_defReal, 0.00288735356009857_defReal,&
    0.00274653572997855_defReal, 0.00261258566331976_defReal,&
    0.00248516841549965_defReal, 0.00236396537733015_defReal,&
    0.00224867347836952_defReal, 0.00213900442908910_defReal,&
    0.00203468400000000_defReal, 0.00193545125447363_defReal,&
    0.00184105814880519_defReal, 0.00175126865088825_defReal,&
    0.00166585824004219_defReal, 0.00158461334559317_defReal,&
    0.00150733081283578_defReal, 0.00143381739504010_defReal,&
    0.00136388927023385_defReal, 0.00129737158155135_defReal,&
    0.00123409800000000_defReal, 0.00117391033438097_defReal,&
    0.00111665805565396_defReal, 0.00106219800332060_defReal,&
    0.00101039399890194_defReal, 0.000961116505421376_defReal,&
    0.000914242303494762_defReal, 0.000869654183217837_defReal,&
    0.000827240651080435_defReal, 0.000786895651174677_defReal,&
    0.000748518299999999_defReal, 0.000712012632002927_defReal,&
    0.000677287366430101_defReal, 0.000644255672031302_defReal,&
    0.000612834952366326_defReal, 0.000582946639270859_defReal,&
    0.000554515996395153_defReal, 0.000527471932324218_defReal,&
    0.000501746822812264_defReal, 0.000477276341686859_defReal,&
    0.000453999299999999_defReal, 0.000436197728282311_defReal,&
    0.000419094166353669_defReal, 0.000402661244851786_defReal,&
    0.000386872667582696_defReal, 0.000371703169441208_defReal,&
    0.000357128475981329_defReal, 0.000343125264571949_defReal,&
    0.000329671127075640_defReal, 0.000316744533990831_defReal,&
    0.000304324799999999_defReal, 0.000283278043596827_defReal,&
    0.000263686856884638_defReal, 0.000245450574322161_defReal,&
    0.000228475492282257_defReal, 0.000212674387573868_defReal,&
    0.000197966069262442_defReal, 0.000184274961485947_defReal,&
    0.000171530715122855_defReal, 0.000159667846316722_defReal,&
    0.000148625400000000_defReal, 0.000141612677792315_defReal,&
    0.000134930842988548_defReal, 0.000128564283038987_defReal,&
    0.000122498122054509_defReal, 0.000116718186048070_defReal,&
    0.000111210969816255_defReal, 0.000105963605383469_defReal,&
    0.000100963831935062_defReal, 9.61999671691203e-05_defReal,&
    9.16608799999997e-05_defReal, 8.89518915934943e-05_defReal,&
    8.63229658940736e-05_defReal, 8.37717366911441e-05_defReal,&
    8.12959077062038e-05_defReal, 7.88932505260366e-05_defReal,&
    7.65616025969902e-05_defReal, 7.42988652785319e-05_defReal,&
    7.21030019543302e-05_defReal, 6.99720361991626e-05_defReal,&
    6.79040499999998e-05_defReal, 6.44310504513163e-05_defReal,&
    6.11356798638679e-05_defReal, 5.80088532817174e-05_defReal,&
    5.50419504052755e-05_defReal, 5.22267918261307e-05_defReal,&
    4.95556164773651e-05_defReal, 4.70210602371906e-05_defReal,&
    4.46161356269150e-05_defReal, 4.23342125472713e-05_defReal,&
    4.01689999999999e-05_defReal, 3.79244319003215e-05_defReal,&
    3.58052860405319e-05_defReal, 3.38045540619803e-05_defReal,&
    3.19156192199037e-05_defReal, 3.01322345007800e-05_defReal,&
    2.84485019624425e-05_defReal, 2.68588532286294e-05_defReal,&
    2.53580310734618e-05_defReal, 2.39410720349459e-05_defReal,&
    2.26032900000000e-05_defReal, 2.15009147901778e-05_defReal,&
    2.04523030414815e-05_defReal, 1.94548326795697e-05_defReal,&
    1.85060095101463e-05_defReal, 1.76034609821790e-05_defReal,&
    1.67449302552881e-05_defReal, 1.59282705564731e-05_defReal,&
    1.51514398120639e-05_defReal, 1.44124955414763e-05_defReal,&
    1.37095900000000e-05_defReal, 1.30409654580515e-05_defReal,&
    1.24049501172603e-05_defReal, 1.17999536082438e-05_defReal,&
    1.12244631248431e-05_defReal, 1.06770396413205e-05_defReal,&
    1.01563143140464e-05_defReal, 9.66098505867735e-06_defReal,&
    9.18981329426793e-06_defReal, 8.74162084617339e-06_defReal,&
    8.31528699999999e-06_defReal, 7.72850546163712e-06_defReal,&
    7.18313110185552e-06_defReal, 6.67624195681351e-06_defReal,&
    6.20512225572541e-06_defReal, 5.76724787051847e-06_defReal,&
    5.36027279225806e-06_defReal, 4.98201656188552e-06_defReal,&
    4.63045258792617e-06_defReal, 4.30369728857696e-06_defReal,&
    3.99999999999999e-06_defReal, 3.27411076822342e-06_defReal,&
    2.67995033064914e-06_defReal, 2.19361355897057e-06_defReal,&
    1.79553344368661e-06_defReal, 1.46969384566990e-06_defReal,&
    1.20298511152488e-06_defReal, 9.84676626914018e-07_defReal,&
    8.05985086849276e-07_defReal, 6.59721112970176e-07_defReal,&
    5.39999999999999e-07_defReal, 4.56199057651903e-07_defReal,&
    3.85402926300897e-07_defReal, 3.25593429249547e-07_defReal,&
    2.75065584446843e-07_defReal, 2.32379000772445e-07_defReal,&
    1.96316816982371e-07_defReal, 1.65851012793664e-07_defReal,&
    1.40113103235340e-07_defReal, 1.18369380853048e-07_defReal,&
    9.99999999999998e-08_defReal, 3.98107568658875e-08_defReal,&
    1.58489636223482e-08_defReal, 6.30959237345600e-09_defReal,&
    2.51189647902516e-09_defReal, 1.00000499998750e-09_defReal,&
    3.98109559191742e-10_defReal, 1.58490428669682e-10_defReal,&
    6.30962392133900e-11_defReal, 2.51190903847615e-11_defReal,&
    1.00001000000000e-11_defReal]

real(defReal),dimension(176),parameter :: vitaminj = [ &
    1.964000E+01_defReal,&
    1.733300E+01_defReal,&
    1.690500E+01_defReal,&
    1.648700E+01_defReal,&
    1.568300E+01_defReal,&
    1.491800E+01_defReal,&
    1.455000E+01_defReal,&
    1.419100E+01_defReal,&
    1.384000E+01_defReal,&
    1.349900E+01_defReal,&
    1.284000E+01_defReal,&
    1.252300E+01_defReal,&
    1.221400E+01_defReal,&
    1.161800E+01_defReal,&
    1.105200E+01_defReal,&
    1.051300E+01_defReal,&
    1.000000E+01_defReal,&
    9.512300E+00_defReal,&
    9.048400E+00_defReal,&
    8.607100E+00_defReal,&
    8.187300E+00_defReal,&
    7.788000E+00_defReal,&
    7.408200E+00_defReal,&
    7.046900E+00_defReal,&
    6.703200E+00_defReal,&
    6.592400E+00_defReal,&
    6.376300E+00_defReal,&
    6.065300E+00_defReal,&
    5.769500E+00_defReal,&
    5.488100E+00_defReal,&
    5.220500E+00_defReal,&
    4.965900E+00_defReal,&
    4.723700E+00_defReal,&
    4.493300E+00_defReal,&
    4.065700E+00_defReal,&
    3.678800E+00_defReal,&
    3.328700E+00_defReal,&
    3.166400E+00_defReal,&
    3.011900E+00_defReal,&
    2.865000E+00_defReal,&
    2.725300E+00_defReal,&
    2.592400E+00_defReal,&
    2.466000E+00_defReal,&
    2.385200E+00_defReal,&
    2.365300E+00_defReal,&
    2.345700E+00_defReal,&
    2.306900E+00_defReal,&
    2.231300E+00_defReal,&
    2.122500E+00_defReal,&
    2.019000E+00_defReal,&
    1.920500E+00_defReal,&
    1.826800E+00_defReal,&
    1.737700E+00_defReal,&
    1.653000E+00_defReal,&
    1.572400E+00_defReal,&
    1.495700E+00_defReal,&
    1.422700E+00_defReal,&
    1.353400E+00_defReal,&
    1.287300E+00_defReal,&
    1.224600E+00_defReal,&
    1.164800E+00_defReal,&
    1.108000E+00_defReal,&
    1.002600E+00_defReal,&
    9.616400E-01_defReal,&
    9.071800E-01_defReal,&
    8.629400E-01_defReal,&
    8.208500E-01_defReal,&
    7.808200E-01_defReal,&
    7.427400E-01_defReal,&
    7.065100E-01_defReal,&
    6.720600E-01_defReal,&
    6.392800E-01_defReal,&
    6.081000E-01_defReal,&
    5.784400E-01_defReal,&
    5.502300E-01_defReal,&
    5.234000E-01_defReal,&
    4.978700E-01_defReal,&
    4.504900E-01_defReal,&
    4.076200E-01_defReal,&
    3.877400E-01_defReal,&
    3.688300E-01_defReal,&
    3.337300E-01_defReal,&
    3.019700E-01_defReal,&
    2.985000E-01_defReal,&
    2.972000E-01_defReal,&
    2.945200E-01_defReal,&
    2.872500E-01_defReal,&
    2.732400E-01_defReal,&
    2.472400E-01_defReal,&
    2.351800E-01_defReal,&
    2.237100E-01_defReal,&
    2.128000E-01_defReal,&
    2.024200E-01_defReal,&
    1.925500E-01_defReal,&
    1.831600E-01_defReal,&
    1.742200E-01_defReal,&
    1.657300E-01_defReal,&
    1.576400E-01_defReal,&
    1.499600E-01_defReal,&
    1.426400E-01_defReal,&
    1.356900E-01_defReal,&
    1.290700E-01_defReal,&
    1.227700E-01_defReal,&
    1.167900E-01_defReal,&
    1.110900E-01_defReal,&
    9.803700E-02_defReal,&
    8.651700E-02_defReal,&
    8.250000E-02_defReal,&
    7.950000E-02_defReal,&
    7.200000E-02_defReal,&
    6.737900E-02_defReal,&
    5.656200E-02_defReal,&
    5.247500E-02_defReal,&
    4.630900E-02_defReal,&
    4.086800E-02_defReal,&
    3.430700E-02_defReal,&
    3.182800E-02_defReal,&
    2.850000E-02_defReal,&
    2.700000E-02_defReal,&
    2.605800E-02_defReal,&
    2.478800E-02_defReal,&
    2.417600E-02_defReal,&
    2.357900E-02_defReal,&
    2.187500E-02_defReal,&
    1.930500E-02_defReal,&
    1.503400E-02_defReal,&
    1.170900E-02_defReal,&
    1.059500E-02_defReal,&
    9.118800E-03_defReal,&
    7.101700E-03_defReal,&
    5.530800E-03_defReal,&
    4.307400E-03_defReal,&
    3.707400E-03_defReal,&
    3.354600E-03_defReal,&
    3.035400E-03_defReal,&
    2.746500E-03_defReal,&
    2.612600E-03_defReal,&
    2.485200E-03_defReal,&
    2.248700E-03_defReal,&
    2.034700E-03_defReal,&
    1.584600E-03_defReal,&
    1.234100E-03_defReal,&
    9.611200E-04_defReal,&
    7.485200E-04_defReal,&
    5.829500E-04_defReal,&
    4.540000E-04_defReal,&
    3.535800E-04_defReal,&
    2.753600E-04_defReal,&
    2.144500E-04_defReal,&
    1.670200E-04_defReal,&
    1.300700E-04_defReal,&
    1.013000E-04_defReal,&
    7.889300E-05_defReal,&
    6.144200E-05_defReal,&
    4.785100E-05_defReal,&
    3.726700E-05_defReal,&
    2.902300E-05_defReal,&
    2.260300E-05_defReal,&
    1.760300E-05_defReal,&
    1.371000E-05_defReal,&
    1.067700E-05_defReal,&
    8.315300E-06_defReal,&
    6.476000E-06_defReal,&
    5.043500E-06_defReal,&
    3.927900E-06_defReal,&
    3.059000E-06_defReal,&
    2.382400E-06_defReal,&
    1.855400E-06_defReal,&
    1.445000E-06_defReal,&
    1.125400E-06_defReal,&
    8.764200E-07_defReal,&
    6.825600E-07_defReal,&
    5.315800E-07_defReal,&
    4.139900E-07_defReal,&
    1.000000E-07_defReal,&
    1.000000E-11_defReal]

end module preDefEnergyGrids
