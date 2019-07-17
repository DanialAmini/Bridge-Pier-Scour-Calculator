function SVMforAll(xx1, xx2, xx3, xx4, xx5, xx6, xx7, cpx, case_){
  
  var x1, x2, x3, x4, x5, x6, x7, z ;
  
  
  if(case_ == 2 ) {
    x1 = fminmax(xx1, 0.248668898615326, 0.59769387755102) ; x2 = fminmax(xx2, 0, 0) ; x3 = fminmax(xx3, 0.01, 0.986838420610473) ; x4 = fminmax(xx4, 1, 1) ; x5 = fminmax(xx5, 0.0131615793895268, 0.99) ; x6 = fminmax(xx6, 0.47244094488189, 1.5) ; x7 = fminmax(xx7, 0, 0);
    z=SVMc2(x1, x2, x3, x4, x5, x6, x7);
    z = fminmax(z, 0.441929789124045, 1.18845467109234);
  }else if( case_ == 3 ) {
    x1 = fminmax(xx1, 0.116617219019459, 0.6851488) ; x2 = fminmax(xx2, 0.177777777777778, 0.85) ; x3 = fminmax(xx3, 0, 0.88) ; x4 = fminmax(xx4, 0.01, 0.954166666666667) ; x5 = fminmax(xx5, 0.01, 0.6) ; x6 = fminmax(xx6, 0.2, 1.5) ; x7 = fminmax(xx7, 0, 1.83333333333333);
    z=SVMc3(x1, x2, x3, x4, x5, x6, x7);
    z = fminmax(z, 0.350480853596402, 1.20687316557003);
  }else if( case_ == 4 ) {
    x1 = fminmax(xx1, 0.116617219019459, 0.6851488) ; x2 = fminmax(xx2, 0.177777777777778, 1) ; x3 = fminmax(xx3, -1.42857142857143, -0.00086887835702996) ; x4 = fminmax(xx4, 0, 1.01) ; x5 = fminmax(xx5, 0.1, 1.59055118110236) ; x6 = fminmax(xx6, 0.2, 4.28571428571429) ; x7 = fminmax(xx7, 0, 1.83333333333333);
    z=SVMc4(x1, x2, x3, x4, x5, x6, x7);
    z = fminmax(z, 0.00378668073195147, 1.74722420826812);
  }else if( case_ == 5 ) {
    x1 = fminmax(xx1, 0, 0.6851488) ; x2 = fminmax(xx2, 0.177777777777778, 0.85) ; x3 = fminmax(xx3, -2.04, -0.1) ; x4 = fminmax(xx4, -1.5, -0.00714285714285714) ; x5 = fminmax(xx5, 0.00833333333333334, 0.769230769230769) ; x6 = fminmax(xx6, 0.2, 2.30769230769231) ; x7 = fminmax(xx7, 0, 1.83333333333333);
    z=SVMc5(x1, x2, x3, x4, x5, x6, x7);
    z = fminmax(z, 0.00869521112019744, 1.17995552612205);
  }else if( case_ == 1 || case_ == 6 ) {
    z = 1;
  }
  
  return z;  
}



function SVMc2(x1, x2, x3, x4, x5, x6, x7){
  var x1s, x2s, x3s, x4s, x5s, x6s, x7s, z ;
  
  x1s = (x1 - 0.438361695166783) / 0.108251416529081;
  x3s = (x3 - 0.670032084854903) / 0.265517143681842;
  x6s = (x6 - 0.654644371879696) / 0.239471358604243;
  ////////////////////////////////////////////////////////////;
  z = +3.870584;
  z = z + 512 * Math.exp(-0.15 * (Math.pow(x1s - -1.101802,2) + Math.pow(x3s - 0.5893019,2) + Math.pow(x6s - -0.1236096,2))) //index=1;
  z = z + 512 * Math.exp(-0.15 * (Math.pow(x1s - -1.101802,2) + Math.pow(x3s - 0.8713379,2) + Math.pow(x6s - -0.08721789,2))) //index=2;
  z = z + 512 * Math.exp(-0.15 * (Math.pow(x1s - -1.101802,2) + Math.pow(x3s - 0.8159858,2) + Math.pow(x6s - -0.2281207,2))) //index=3;
  z = z + -512 * Math.exp(-0.15 * (Math.pow(x1s - -1.101802,2) + Math.pow(x3s - 0.684326,2) + Math.pow(x6s - -0.3088543,2))) //index=4;
  z = z + -482.1809 * Math.exp(-0.15 * (Math.pow(x1s - -1.243363,2) + Math.pow(x3s - 0.5403249,2) + Math.pow(x6s - -0.2278868,2))) //index=5;
  z = z + -384.0999 * Math.exp(-0.15 * (Math.pow(x1s - 0.4314383,2) + Math.pow(x3s - 0.3940077,2) + Math.pow(x6s - -0.08703151,2))) //index=7;
  z = z + 488.3062 * Math.exp(-0.15 * (Math.pow(x1s - 0.4314383,2) + Math.pow(x3s - 0.8183722,2) + Math.pow(x6s - -0.08703151,2))) //index=8;
  z = z + -274.6408 * Math.exp(-0.15 * (Math.pow(x1s - -0.9696939,2) + Math.pow(x3s - 0.8661132,2) + Math.pow(x6s - -0.2281875,2))) //index=9;
  z = z + -512 * Math.exp(-0.15 * (Math.pow(x1s - -0.9696939,2) + Math.pow(x3s - 0.8557948,2) + Math.pow(x6s - -0.1595432,2))) //index=10;
  z = z + -319.6462 * Math.exp(-0.15 * (Math.pow(x1s - 0.4125742,2) + Math.pow(x3s - 0.8333634,2) + Math.pow(x6s - -0.0103163,2))) //index=11;
  z = z + -1.406407 * Math.exp(-0.15 * (Math.pow(x1s - -1.178563,2) + Math.pow(x3s - 0.2635156,2) + Math.pow(x6s - 3.446573,2))) //index=12;
  z = z + -12.21394 * Math.exp(-0.15 * (Math.pow(x1s - 0.06673807,2) + Math.pow(x3s - 0.1128662,2) + Math.pow(x6s - 1.442158,2))) //index=13;
  z = z + -7.646453 * Math.exp(-0.15 * (Math.pow(x1s - 0.06673807,2) + Math.pow(x3s - -1.017004,2) + Math.pow(x6s - 1.442158,2))) //index=15;
  z = z + 236.2812 * Math.exp(-0.15 * (Math.pow(x1s - 0.7275768,2) + Math.pow(x3s - 0.0291721,2) + Math.pow(x6s - 0.05020348,2))) //index=16;
  z = z + 293.6641 * Math.exp(-0.15 * (Math.pow(x1s - 1.471872,2) + Math.pow(x3s - -0.8286926,2) + Math.pow(x6s - -0.6875326,2))) //index=17;
  z = z + -24.37327 * Math.exp(-0.15 * (Math.pow(x1s - 1.163202,2) + Math.pow(x3s - 0.886872,2) + Math.pow(x6s - -0.7608569,2))) //index=18;
  z = z + -303.1856 * Math.exp(-0.15 * (Math.pow(x1s - 1.163202,2) + Math.pow(x3s - -0.5958977,2) + Math.pow(x6s - -0.7608569,2))) //index=19;
  z = z + -29.55968 * Math.exp(-0.15 * (Math.pow(x1s - 1.163202,2) + Math.pow(x3s - -0.8924517,2) + Math.pow(x6s - -0.7608569,2))) //index=20;
  z = z + -91.63715 * Math.exp(-0.15 * (Math.pow(x1s - 1.163202,2) + Math.pow(x3s - -1.782114,2) + Math.pow(x6s - -0.7608569,2))) //index=21;
  z = z + 46.7347 * Math.exp(-0.15 * (Math.pow(x1s - 1.163202,2) + Math.pow(x3s - -2.226944,2) + Math.pow(x6s - -0.7608569,2))) //index=23;
  z = z + 353.604 * Math.exp(-0.15 * (Math.pow(x1s - -0.6424424,2) + Math.pow(x3s - 0.3388403,2) + Math.pow(x6s - -0.7292913,2))) //index=24;
  //////////////////////////////////////////////////////////////////////////////;
  z = z * 0.155269560672747 + 0.680481405229821;
 
  return z;
}




function SVMc3(x1, x2, x3, x4, x5, x6, x7){
  var x1s, x2s, x3s, x4s, x5s, x6s, x7s, z ;
  
     x1s = (x1 - 0.351588927683803) / 0.109774539514352;
    x2s = (x2 - 0.392897165561354) / 0.171979131034122;
    x3s = (x3 - 0.277785732742744) / 0.190119711087647;
    x4s = (x4 - 0.511709488688356) / 0.21102496136652;
    x5s = (x5 - 0.233923755945611) / 0.117358198417105;
    x6s = (x6 - 0.776029438712898) / 0.365841266818521;
    x7s = (x7 - 0.715999338688565) / 0.361703138905198;
     ////////////////////////////////////////////////////////////;
    z = +1.302695;
    z = z + 16 * Math.exp(-0.05 * (Math.pow(x1s - -0.2885733,2) + Math.pow(x2s - 0.5396999,2) + Math.pow(x3s - -0.9735692,2) + Math.pow(x4s - -1.456655,2) + Math.pow(x5s - -1.042073,2) + Math.pow(x6s - -0.8498581,2) + Math.pow(x7s - 0.4192602,2))) //index=1;
    z = z + 16 * Math.exp(-0.05 * (Math.pow(x1s - -0.296051,2) + Math.pow(x2s - -0.8632019,2) + Math.pow(x3s - -1.287948,2) + Math.pow(x4s - -1.271219,2) + Math.pow(x5s - -0.1993441,2) + Math.pow(x6s - -0.5027218,2) + Math.pow(x7s - -0.09450058,2))) //index=2;
    z = z + -3.168136 * Math.exp(-0.05 * (Math.pow(x1s - -0.296051,2) + Math.pow(x2s - -0.8632019,2) + Math.pow(x3s - -1.287948,2) + Math.pow(x4s - -1.271219,2) + Math.pow(x5s - -0.1993441,2) + Math.pow(x6s - -0.5027218,2) + Math.pow(x7s - -0.09450058,2))) //index=3;
    z = z + 16 * Math.exp(-0.05 * (Math.pow(x1s - -0.296051,2) + Math.pow(x2s - -0.8632019,2) + Math.pow(x3s - -0.01682853,2) + Math.pow(x4s - -0.05585571,2) + Math.pow(x5s - -0.07317352,2) + Math.pow(x6s - -0.3888879,2) + Math.pow(x7s - -0.09450058,2))) //index=4;
    z = z + 5.025691 * Math.exp(-0.05 * (Math.pow(x1s - -0.296051,2) + Math.pow(x2s - -0.8632019,2) + Math.pow(x3s - 1.346268,2) + Math.pow(x4s - 1.129029,2) + Math.pow(x5s - -0.1508099,2) + Math.pow(x6s - -0.4589331,2) + Math.pow(x7s - -0.09450058,2))) //index=5;
    z = z + 15.24877 * Math.exp(-0.05 * (Math.pow(x1s - -0.4356477,2) + Math.pow(x2s - 0.4289445,2) + Math.pow(x3s - 1.56897,2) + Math.pow(x4s - 1.623185,2) + Math.pow(x5s - 0.3769686,2) + Math.pow(x6s - -0.4919168,2) + Math.pow(x7s - -0.3338684,2))) //index=6;
    z = z + -16 * Math.exp(-0.05 * (Math.pow(x1s - -0.4356477,2) + Math.pow(x2s - 0.4289445,2) + Math.pow(x3s - 1.274127,2) + Math.pow(x4s - 1.36641,2) + Math.pow(x5s - 0.3928984,2) + Math.pow(x6s - -0.4809666,2) + Math.pow(x7s - -0.3338684,2))) //index=7;
    z = z + -16 * Math.exp(-0.05 * (Math.pow(x1s - -0.4356477,2) + Math.pow(x2s - 0.4289445,2) + Math.pow(x3s - 1.843457,2) + Math.pow(x4s - 1.897038,2) + Math.pow(x5s - 0.4247227,2) + Math.pow(x6s - -0.4590904,2) + Math.pow(x7s - -0.3338684,2))) //index=8;
    z = z + -16 * Math.exp(-0.05 * (Math.pow(x1s - -0.1657759,2) + Math.pow(x2s - -1.250846,2) + Math.pow(x3s - -0.7848439,2) + Math.pow(x4s - -0.6986082,2) + Math.pow(x5s - 0.01525696,2) + Math.pow(x6s - -0.3640166,2) + Math.pow(x7s - 0.09400157,2))) //index=9;
    z = z + -16 * Math.exp(-0.05 * (Math.pow(x1s - -0.1657759,2) + Math.pow(x2s - -1.250846,2) + Math.pow(x3s - -0.4467111,2) + Math.pow(x4s - -0.3939726,2) + Math.pow(x5s - 0.01525696,2) + Math.pow(x6s - -0.3640166,2) + Math.pow(x7s - 0.09400157,2))) //index=10;
    z = z + -14.35375 * Math.exp(-0.05 * (Math.pow(x1s - 1.215916,2) + Math.pow(x2s - -1.250846,2) + Math.pow(x3s - 1.672414,2) + Math.pow(x4s - 1.507299,2) + Math.pow(x5s - 0.001012262,2) + Math.pow(x6s - -0.376479,2) + Math.pow(x7s - 0.09400157,2))) //index=11;
    z = z + 16 * Math.exp(-0.05 * (Math.pow(x1s - 1.215916,2) + Math.pow(x2s - -1.250846,2) + Math.pow(x3s - 0.8682498,2) + Math.pow(x4s - 0.7907213,2) + Math.pow(x5s - 0.01525696,2) + Math.pow(x6s - -0.3640166,2) + Math.pow(x7s - 0.09400157,2))) //index=12;
    z = z + -16 * Math.exp(-0.05 * (Math.pow(x1s - 1.215916,2) + Math.pow(x2s - -1.250846,2) + Math.pow(x3s - -0.349875,2) + Math.pow(x4s - -0.3224619,2) + Math.pow(x5s - -0.01303181,2) + Math.pow(x6s - -0.3887659,2) + Math.pow(x7s - 0.09400157,2))) //index=13;
    z = z + -15.26501 * Math.exp(-0.05 * (Math.pow(x1s - -0.1536355,2) + Math.pow(x2s - -1.250846,2) + Math.pow(x3s - 0.4549764,2) + Math.pow(x4s - 0.07990495,2) + Math.pow(x5s - -0.5933803,2) + Math.pow(x6s - -0.3640166,2) + Math.pow(x7s - 0.6123825,2))) //index=14;
    z = z + -16 * Math.exp(-0.05 * (Math.pow(x1s - -0.1536355,2) + Math.pow(x2s - -1.250846,2) + Math.pow(x3s - -0.8975549,2) + Math.pow(x4s - -1.138637,2) + Math.pow(x5s - -0.5933803,2) + Math.pow(x6s - -0.3640166,2) + Math.pow(x7s - 0.6123825,2))) //index=15;
    z = z + 0.9854337 * Math.exp(-0.05 * (Math.pow(x1s - 1.233511,2) + Math.pow(x2s - -1.250846,2) + Math.pow(x3s - 0.1660364,2) + Math.pow(x4s - -0.1748103,2) + Math.pow(x5s - -0.5833093,2) + Math.pow(x6s - -0.3513749,2) + Math.pow(x7s - 0.6123825,2))) //index=16;
    z = z + 16 * Math.exp(-0.05 * (Math.pow(x1s - 1.197313,2) + Math.pow(x2s - -1.250846,2) + Math.pow(x3s - -0.1837189,2) + Math.pow(x4s - -0.1570338,2) + Math.pow(x5s - 0.01525696,2) + Math.pow(x6s - -0.3640166,2) + Math.pow(x7s - 0.09400157,2))) //index=18;
    z = z + 16 * Math.exp(-0.05 * (Math.pow(x1s - -0.1106845,2) + Math.pow(x2s - -1.250846,2) + Math.pow(x3s - 0.642828,2) + Math.pow(x4s - 0.5876309,2) + Math.pow(x5s - 0.01525696,2) + Math.pow(x6s - -0.3640166,2) + Math.pow(x7s - 0.09400157,2))) //index=19;
    z = z + -1.984166 * Math.exp(-0.05 * (Math.pow(x1s - 1.226852,2) + Math.pow(x2s - -1.250846,2) + Math.pow(x3s - -1.088071,2) + Math.pow(x4s - -1.315801,2) + Math.pow(x5s - -0.6033084,2) + Math.pow(x6s - -0.376479,2) + Math.pow(x7s - 0.6123825,2))) //index=21;
    z = z + 1.731005 * Math.exp(-0.05 * (Math.pow(x1s - -0.3717474,2) + Math.pow(x2s - -0.3594395,2) + Math.pow(x3s - -1.040322,2) + Math.pow(x4s - 0.3236134,2) + Math.pow(x5s - 2.267215,2) + Math.pow(x6s - 1.924251,2) + Math.pow(x7s - 0.8698097,2))) //index=22;
    z = z + 0.9114 * Math.exp(-0.05 * (Math.pow(x1s - -0.3717474,2) + Math.pow(x2s - -0.3594395,2) + Math.pow(x3s - 3.167553,2) + Math.pow(x4s - 1.792634,2) + Math.pow(x5s - -1.908037,2) + Math.pow(x6s - 1.924251,2) + Math.pow(x7s - 0.8698097,2))) //index=23;
    z = z + -3.959535 * Math.exp(-0.05 * (Math.pow(x1s - -0.3717474,2) + Math.pow(x2s - -0.3594395,2) + Math.pow(x3s - -1.46111,2) + Math.pow(x4s - -2.377489,2) + Math.pow(x5s - -1.908037,2) + Math.pow(x6s - 1.924251,2) + Math.pow(x7s - 0.8698097,2))) //index=26;
    z = z + 2.416122 * Math.exp(-0.05 * (Math.pow(x1s - -0.3717474,2) + Math.pow(x2s - -0.3594395,2) + Math.pow(x3s - 0.3798358,2) + Math.pow(x4s - -0.7189173,2) + Math.pow(x5s - -1.908037,2) + Math.pow(x6s - 1.924251,2) + Math.pow(x7s - 0.8698097,2))) //index=27;
    z = z + 2.478317 * Math.exp(-0.05 * (Math.pow(x1s - 1.264676,2) + Math.pow(x2s - 2.367164,2) + Math.pow(x3s - -0.2776447,2) + Math.pow(x4s - -0.766305,2) + Math.pow(x5s - -0.9281308,2) + Math.pow(x6s - 0.156636,2) + Math.pow(x7s - -1.633935,2))) //index=28;
    z = z + -15.39498 * Math.exp(-0.05 * (Math.pow(x1s - 1.417746,2) + Math.pow(x2s - 1.97952,2) + Math.pow(x3s - 0.1168436,2) + Math.pow(x4s - -0.4108968,2) + Math.pow(x5s - -0.9281308,2) + Math.pow(x6s - -0.4128278,2) + Math.pow(x7s - -1.47685,2))) //index=29;
    z = z + 0.0580578 * Math.exp(-0.05 * (Math.pow(x1s - -0.6966541,2) + Math.pow(x2s - -0.08353393,2) + Math.pow(x3s - -0.3214767,2) + Math.pow(x4s - 0.793542,2) + Math.pow(x5s - 1.94768,2) + Math.pow(x6s - -0.1053174,2) + Math.pow(x7s - 1.899308,2))) //index=31;
    z = z + -16 * Math.exp(-0.05 * (Math.pow(x1s - -2.140494,2) + Math.pow(x2s - -0.01911148,2) + Math.pow(x3s - -1.132369,2) + Math.pow(x4s - -1.437632,2) + Math.pow(x5s - -0.7506116,2) + Math.pow(x6s - -1.244245,2) + Math.pow(x7s - 0.4165497,2))) //index=32;
    z = z + 16 * Math.exp(-0.05 * (Math.pow(x1s - -2.140494,2) + Math.pow(x2s - -0.01911148,2) + Math.pow(x3s - -0.03656854,2) + Math.pow(x4s - -0.4503866,2) + Math.pow(x5s - -0.7506116,2) + Math.pow(x6s - -1.244245,2) + Math.pow(x7s - 0.4165497,2))) //index=33;
    z = z + -1.137507 * Math.exp(-0.05 * (Math.pow(x1s - -2.140494,2) + Math.pow(x2s - -0.01911148,2) + Math.pow(x3s - 1.059232,2) + Math.pow(x4s - 0.5368583,2) + Math.pow(x5s - -0.7506116,2) + Math.pow(x6s - -1.244245,2) + Math.pow(x7s - 0.4165497,2))) //index=34;
    z = z + -1.44328 * Math.exp(-0.05 * (Math.pow(x1s - 0.8909401,2) + Math.pow(x2s - 0.551856,2) + Math.pow(x3s - 0.2483397,2) + Math.pow(x4s - 0.5763481,2) + Math.pow(x5s - 0.6340382,2) + Math.pow(x6s - -0.7203382,2) + Math.pow(x7s - 3.089091,2))) //index=35;
    z = z + 3.584337 * Math.exp(-0.05 * (Math.pow(x1s - 0.8562756,2) + Math.pow(x2s - 0.9134994,2) + Math.pow(x3s - -0.3302432,2) + Math.pow(x4s - 0.7264094,2) + Math.pow(x5s - 1.841169,2) + Math.pow(x6s - 0.6122069,2) + Math.pow(x7s - -0.8485092,2))) //index=37;
    z = z + 16 * Math.exp(-0.05 * (Math.pow(x1s - 0.8562756,2) + Math.pow(x2s - 0.9134994,2) + Math.pow(x3s - 0.4587334,2) + Math.pow(x4s - 0.7264094,2) + Math.pow(x5s - 0.5630305,2) + Math.pow(x6s - 0.6122069,2) + Math.pow(x7s - -0.8485092,2))) //index=38;
    z = z + -16 * Math.exp(-0.05 * (Math.pow(x1s - 0.8562756,2) + Math.pow(x2s - 0.9134994,2) + Math.pow(x3s - -1.277015,2) + Math.pow(x4s - -0.8373867,2) + Math.pow(x5s - 0.5630305,2) + Math.pow(x6s - 0.6122069,2) + Math.pow(x7s - -0.8485092,2))) //index=39;
    z = z + 1.799366 * Math.exp(-0.05 * (Math.pow(x1s - 1.507945,2) + Math.pow(x2s - 2.027976,2) + Math.pow(x3s - 0.3506144,2) + Math.pow(x4s - 0.7343073,2) + Math.pow(x5s - 0.7523843,2) + Math.pow(x6s - -0.298935,2) + Math.pow(x7s - -1.513562,2))) //index=40;
    z = z + 9.911787 * Math.exp(-0.05 * (Math.pow(x1s - 1.507945,2) + Math.pow(x2s - 2.027976,2) + Math.pow(x3s - -1.46111,2) + Math.pow(x4s - -0.8979377,2) + Math.pow(x5s - 0.7523843,2) + Math.pow(x6s - -0.298935,2) + Math.pow(x7s - -1.513562,2))) //index=41;
    z = z + -7.449265 * Math.exp(-0.05 * (Math.pow(x1s - 0.8562756,2) + Math.pow(x2s - 0.04130056,2) + Math.pow(x3s - -0.3039439,2) + Math.pow(x4s - 0.7501033,2) + Math.pow(x5s - 1.841169,2) + Math.pow(x6s - 0.6122069,2) + Math.pow(x7s - 0.09400157,2))) //index=42;
    z = z + 16 * Math.exp(-0.05 * (Math.pow(x1s - -0.3717474,2) + Math.pow(x2s - -0.3594395,2) + Math.pow(x3s - -1.040322,2) + Math.pow(x4s - -0.8610805,2) + Math.pow(x5s - 0.1369844,2) + Math.pow(x6s - 1.924251,2) + Math.pow(x7s - 0.8415985,2))) //index=44;
    z = z + -16 * Math.exp(-0.05 * (Math.pow(x1s - -0.7208809,2) + Math.pow(x2s - 0.6227665,2) + Math.pow(x3s - -0.5515516,2) + Math.pow(x4s - 0.2676097,2) + Math.pow(x5s - 1.374707,2) + Math.pow(x6s - 1.119998,2) + Math.pow(x7s - -0.597173,2))) //index=45;
    z = z + -0.2410918 * Math.exp(-0.05 * (Math.pow(x1s - -0.7208809,2) + Math.pow(x2s - 0.6227665,2) + Math.pow(x3s - -0.5469656,2) + Math.pow(x4s - 0.08811063,2) + Math.pow(x5s - 1.044516,2) + Math.pow(x6s - 0.8022312,2) + Math.pow(x7s - -0.597173,2))) //index=46;
    z = z + -16 * Math.exp(-0.05 * (Math.pow(x1s - -0.7208809,2) + Math.pow(x2s - 0.6227665,2) + Math.pow(x3s - -0.7352511,2) + Math.pow(x4s - -0.8231703,2) + Math.pow(x5s - -0.2890617,2) + Math.pow(x6s - -0.4811634,2) + Math.pow(x7s - -0.597173,2))) //index=47;
    z = z + -16 * Math.exp(-0.05 * (Math.pow(x1s - -0.7208809,2) + Math.pow(x2s - 0.6227665,2) + Math.pow(x3s - -0.6195346,2) + Math.pow(x4s - -0.7189173,2) + Math.pow(x5s - -0.2890617,2) + Math.pow(x6s - -0.4811634,2) + Math.pow(x7s - -0.597173,2))) //index=48;
    z = z + -16 * Math.exp(-0.05 * (Math.pow(x1s - -0.7208809,2) + Math.pow(x2s - 0.6227665,2) + Math.pow(x3s - -0.1132746,2) + Math.pow(x4s - -0.02587129,2) + Math.pow(x5s - 0.1369844,2) + Math.pow(x6s - -0.07114954,2) + Math.pow(x7s - -0.597173,2))) //index=49;
    z = z + 16 * Math.exp(-0.05 * (Math.pow(x1s - -0.8323185,2) + Math.pow(x2s - 0.7757838,2) + Math.pow(x3s - -0.2747457,2) + Math.pow(x4s - -0.207246,2) + Math.pow(x5s - 0.07243198,2) + Math.pow(x6s - -0.5474283,2) + Math.pow(x7s - -0.8736428,2))) //index=50;
    z = z + 1.162252 * Math.exp(-0.05 * (Math.pow(x1s - 0.009736165,2) + Math.pow(x2s - -0.8308983,2) + Math.pow(x3s - -0.908826,2) + Math.pow(x4s - -1.453428,2) + Math.pow(x5s - -1.141154,2) + Math.pow(x6s - -1.574534,2) + Math.pow(x7s - -1.979522,2))) //index=51;
    z = z + 16 * Math.exp(-0.05 * (Math.pow(x1s - -0.9375583,2) + Math.pow(x2s - -0.4238722,2) + Math.pow(x3s - 0.06424514,2) + Math.pow(x4s - 1.318756,2) + Math.pow(x5s - 2.267215,2) + Math.pow(x6s - 1.97892,2) + Math.pow(x7s - 0.9579697,2))) //index=52;
    z = z + -11.37246 * Math.exp(-0.05 * (Math.pow(x1s - -0.9375583,2) + Math.pow(x2s - -0.4238722,2) + Math.pow(x3s - 0.5376311,2) + Math.pow(x4s - 1.745246,2) + Math.pow(x5s - 2.267215,2) + Math.pow(x6s - 1.97892,2) + Math.pow(x7s - 0.9579697,2))) //index=53;
    z = z + 16 * Math.exp(-0.05 * (Math.pow(x1s - 0.156935,2) + Math.pow(x2s - -0.8308983,2) + Math.pow(x3s - -1.46111,2) + Math.pow(x4s - -1.28757,2) + Math.pow(x5s - 0.0517752,2) + Math.pow(x6s - -0.8091745,2) + Math.pow(x7s - 0.4165497,2))) //index=54;
    z = z + 0.9808393 * Math.exp(-0.05 * (Math.pow(x1s - 3.03859,2) + Math.pow(x2s - 1.204232,2) + Math.pow(x3s - -0.1987471,2) + Math.pow(x4s - -0.1502642,2) + Math.pow(x5s - 0.0517752,2) + Math.pow(x6s - -1.574534,2) + Math.pow(x7s - -1.057956,2))) //index=55;
    z = z + -16 * Math.exp(-0.05 * (Math.pow(x1s - -0.8018479,2) + Math.pow(x2s - 0.8103361,2) + Math.pow(x3s - -0.2321741,2) + Math.pow(x4s - -0.1662077,2) + Math.pow(x5s - 0.07725833,2) + Math.pow(x6s - -0.5373647,2) + Math.pow(x7s - -0.8903985,2))) //index=56;
    z = z + 13.47581 * Math.exp(-0.05 * (Math.pow(x1s - -0.8018479,2) + Math.pow(x2s - 0.8103361,2) + Math.pow(x3s - -1.06785,2) + Math.pow(x4s - -0.9190973,2) + Math.pow(x5s - 0.07725833,2) + Math.pow(x6s - -0.5373647,2) + Math.pow(x7s - -0.8903985,2))) //index=57;
    z = z + 16 * Math.exp(-0.05 * (Math.pow(x1s - -0.8018479,2) + Math.pow(x2s - 0.8103361,2) + Math.pow(x3s - -1.06785,2) + Math.pow(x4s - -0.9190973,2) + Math.pow(x5s - 0.07725833,2) + Math.pow(x6s - -0.5373647,2) + Math.pow(x7s - -0.8903985,2))) //index=58;
    z = z + 16 * Math.exp(-0.05 * (Math.pow(x1s - -0.8018479,2) + Math.pow(x2s - 0.8103361,2) + Math.pow(x3s - 0.8819118,2) + Math.pow(x4s - 0.806107,2) + Math.pow(x5s - 0.02079003,2) + Math.pow(x6s - -0.5805607,2) + Math.pow(x7s - -0.8903985,2))) //index=59;
    //////////////////////////////////////////////////////////////////////////////;
    z = z * 0.203452859029402 + 0.740579644922011;

 
  return z;
}

function SVMc4(x1, x2, x3, x4, x5, x6, x7){
  var x1s, x2s, x3s, x4s, x5s, x6s, x7s, z ;
  x1s = (x1 - 0.409454328677743) / 0.112095721132887;
    x2s = (x2 - 0.537879566895254) / 0.27559755287981;
    x3s = (x3 - -0.254128837577973) / 0.199340745338229;
    x4s = (x4 - 0.251391996006311) / 0.354810062997544;
    x5s = (x5 - 0.505131223194674) / 0.381491987970682;
    x6s = (x6 - 0.821360076295709) / 0.460694854196827;
    x7s = (x7 - 0.578621664008704) / 0.488635170962942;
     ////////////////////////////////////////////////////////////;
    z = +0.3344353;
    z = z + -32 * Math.exp(-0.05 * (Math.pow(x1s - -0.8061348,2) + Math.pow(x2s - -1.064723,2) + Math.pow(x3s - 0.2649137,2) + Math.pow(x4s - -0.6706582,2) + Math.pow(x5s - -0.7611556,2) + Math.pow(x6s - -0.4718037,2) + Math.pow(x7s - 0.2111934,2))) //index=2;
    z = z + 32 * Math.exp(-0.05 * (Math.pow(x1s - -0.8061348,2) + Math.pow(x2s - -1.064723,2) + Math.pow(x3s - 0.4215808,2) + Math.pow(x4s - -0.5743959,2) + Math.pow(x5s - -0.7534891,2) + Math.pow(x6s - -0.4539487,2) + Math.pow(x7s - 0.2111934,2))) //index=3;
    z = z + 27.54224 * Math.exp(-0.05 * (Math.pow(x1s - -0.8061348,2) + Math.pow(x2s - -1.064723,2) + Math.pow(x3s - 0.4561155,2) + Math.pow(x4s - -0.5549935,2) + Math.pow(x5s - -0.7534891,2) + Math.pow(x6s - -0.4539487,2) + Math.pow(x7s - 0.2111934,2))) //index=4;
    z = z + -32 * Math.exp(-0.05 * (Math.pow(x1s - -0.8061348,2) + Math.pow(x2s - -1.064723,2) + Math.pow(x3s - 0.4504401,2) + Math.pow(x4s - -0.5539293,2) + Math.pow(x5s - -0.7495338,2) + Math.pow(x6s - -0.4447368,2) + Math.pow(x7s - 0.2111934,2))) //index=5;
    z = z + -32 * Math.exp(-0.05 * (Math.pow(x1s - -0.8061348,2) + Math.pow(x2s - -1.064723,2) + Math.pow(x3s - 1.015976,2) + Math.pow(x4s - -0.2720622,2) + Math.pow(x5s - -0.7828901,2) + Math.pow(x6s - -0.5224227,2) + Math.pow(x7s - 0.2111934,2))) //index=7;
    z = z + 32 * Math.exp(-0.05 * (Math.pow(x1s - -0.8061348,2) + Math.pow(x2s - -1.064723,2) + Math.pow(x3s - 1.141269,2) + Math.pow(x4s - -0.1822957,2) + Math.pow(x5s - -0.7648711,2) + Math.pow(x6s - -0.480457,2) + Math.pow(x7s - 0.2111934,2))) //index=8;
    z = z + 29.36489 * Math.exp(-0.05 * (Math.pow(x1s - -0.9428409,2) + Math.pow(x2s - -0.2583945,2) + Math.pow(x3s - 0.6599625,2) + Math.pow(x4s - -0.2487222,2) + Math.pow(x5s - -0.5751548,2) + Math.pow(x6s - -0.4539126,2) + Math.pow(x7s - 0.0340058,2))) //index=9;
    z = z + 32 * Math.exp(-0.05 * (Math.pow(x1s - -0.9428409,2) + Math.pow(x2s - -0.2583945,2) + Math.pow(x3s - 0.6018275,2) + Math.pow(x4s - -0.2922779,2) + Math.pow(x5s - -0.585287,2) + Math.pow(x6s - -0.4718917,2) + Math.pow(x7s - 0.0340058,2))) //index=10;
    z = z + 32 * Math.exp(-0.05 * (Math.pow(x1s - -0.9428409,2) + Math.pow(x2s - -0.2583945,2) + Math.pow(x3s - 0.8704695,2) + Math.pow(x4s - -0.1413485,2) + Math.pow(x5s - -0.585287,2) + Math.pow(x6s - -0.4718917,2) + Math.pow(x7s - 0.0340058,2))) //index=11;
    z = z + -32 * Math.exp(-0.05 * (Math.pow(x1s - -0.9428409,2) + Math.pow(x2s - -0.2583945,2) + Math.pow(x3s - 1.044882,2) + Math.pow(x4s - -0.06407455,2) + Math.pow(x5s - -0.6045532,2) + Math.pow(x6s - -0.5060787,2) + Math.pow(x7s - 0.0340058,2))) //index=12;
    z = z + -32 * Math.exp(-0.05 * (Math.pow(x1s - -0.01582835,2) + Math.pow(x2s - -1.306622,2) + Math.pow(x3s - 0.4445233,2) + Math.pow(x4s - -0.5335898,2) + Math.pow(x5s - -0.7275252,2) + Math.pow(x6s - -0.4355821,2) + Math.pow(x7s - 0.3507286,2))) //index=13;
    z = z + -32 * Math.exp(-0.05 * (Math.pow(x1s - -0.001056515,2) + Math.pow(x2s - -1.306622,2) + Math.pow(x3s - 0.8090252,2) + Math.pow(x4s - -0.5072106,2) + Math.pow(x5s - -0.8934539,2) + Math.pow(x6s - -0.3874646,2) + Math.pow(x7s - 0.7344505,2))) //index=14;
    z = z + 32 * Math.exp(-0.05 * (Math.pow(x1s - 0.6917541,2) + Math.pow(x2s - -1.306622,2) + Math.pow(x3s - 0.8479072,2) + Math.pow(x4s - -0.4886496,2) + Math.pow(x5s - -0.8965081,2) + Math.pow(x6s - -0.3973611,2) + Math.pow(x7s - 0.7344505,2))) //index=15;
    z = z + 21.47139 * Math.exp(-0.05 * (Math.pow(x1s - 0.6917541,2) + Math.pow(x2s - -1.306622,2) + Math.pow(x3s - 0.8090252,2) + Math.pow(x4s - -0.5072106,2) + Math.pow(x5s - -0.8934539,2) + Math.pow(x6s - -0.3874646,2) + Math.pow(x7s - 0.7344505,2))) //index=16;
    z = z + -32 * Math.exp(-0.05 * (Math.pow(x1s - -0.6029966,2) + Math.pow(x2s - -1.306622,2) + Math.pow(x3s - 0.8090252,2) + Math.pow(x4s - -0.5072106,2) + Math.pow(x5s - -0.8934539,2) + Math.pow(x6s - -0.3874646,2) + Math.pow(x7s - 0.7344505,2))) //index=17;
    z = z + -32 * Math.exp(-0.05 * (Math.pow(x1s - 0.6852333,2) + Math.pow(x2s - -1.306622,2) + Math.pow(x3s - 0.9190637,2) + Math.pow(x4s - -0.4486722,2) + Math.pow(x5s - -0.8965081,2) + Math.pow(x6s - -0.3973611,2) + Math.pow(x7s - 0.7344505,2))) //index=19;
    z = z + 10.62649 * Math.exp(-0.05 * (Math.pow(x1s - -0.8802637,2) + Math.pow(x2s - -0.750364,2) + Math.pow(x3s - -0.07961826,2) + Math.pow(x4s - -0.0602914,2) + Math.pow(x5s - -0.01345041,2) + Math.pow(x6s - 1.429666,2) + Math.pow(x7s - 0.9250062,2))) //index=20;
    z = z + -32 * Math.exp(-0.05 * (Math.pow(x1s - 0.7222744,2) + Math.pow(x2s - 0.9510986,2) + Math.pow(x3s - 0.6477794,2) + Math.pow(x4s - -0.7085256,2) + Math.pow(x5s - -0.996433,2) + Math.pow(x6s - 0.02598956,2) + Math.pow(x7s - -0.9283443,2))) //index=22;
    z = z + -32 * Math.exp(-0.05 * (Math.pow(x1s - -1.198443,2) + Math.pow(x2s - -0.5781927,2) + Math.pow(x3s - -0.3555277,2) + Math.pow(x4s - -0.3209943,2) + Math.pow(x5s - -0.1117487,2) + Math.pow(x6s - -0.1820295,2) + Math.pow(x7s - 1.687073,2))) //index=23;
    z = z + 12.80733 * Math.exp(-0.05 * (Math.pow(x1s - -1.198443,2) + Math.pow(x2s - -0.5781927,2) + Math.pow(x3s - 1.253944,2) + Math.pow(x4s - 0.5832454,2) + Math.pow(x5s - -0.1117487,2) + Math.pow(x6s - -0.1820295,2) + Math.pow(x7s - 1.687073,2))) //index=24;
    z = z + -8.074302 * Math.exp(-0.05 * (Math.pow(x1s - 0.3562771,2) + Math.pow(x2s - -0.181695,2) + Math.pow(x3s - -0.2301143,2) + Math.pow(x4s - -0.6850388,2) + Math.pow(x5s - -0.5158638,2) + Math.pow(x6s - -0.6704222,2) + Math.pow(x7s - 2.567788,2))) //index=25;
    z = z + 32 * Math.exp(-0.05 * (Math.pow(x1s - 0.3562771,2) + Math.pow(x2s - -0.181695,2) + Math.pow(x3s - 0.4805616,2) + Math.pow(x4s - -0.2857641,2) + Math.pow(x5s - -0.5158638,2) + Math.pow(x6s - -0.6704222,2) + Math.pow(x7s - 2.567788,2))) //index=27;
    z = z + -32 * Math.exp(-0.05 * (Math.pow(x1s - 0.3562771,2) + Math.pow(x2s - -0.181695,2) + Math.pow(x3s - 1.128531,2) + Math.pow(x4s - 0.07828039,2) + Math.pow(x5s - -0.5158638,2) + Math.pow(x6s - -0.6704222,2) + Math.pow(x7s - 2.567788,2))) //index=28;
    z = z + 8.95226 * Math.exp(-0.05 * (Math.pow(x1s - 0.3562771,2) + Math.pow(x2s - -0.181695,2) + Math.pow(x3s - 0.877704,2) + Math.pow(x4s - -0.06264008,2) + Math.pow(x5s - -0.5158638,2) + Math.pow(x6s - -0.6704222,2) + Math.pow(x7s - 2.567788,2))) //index=29;
    z = z + -12.23371 * Math.exp(-0.05 * (Math.pow(x1s - 0.3223304,2) + Math.pow(x2s - 1.132523,2) + Math.pow(x3s - -1.484248,2) + Math.pow(x4s - -0.5676051,2) + Math.pow(x5s - 0.2486783,2) + Math.pow(x6s - 0.3877619,2) + Math.pow(x7s - -1.003584,2))) //index=31;
    z = z + -0.218417 * Math.exp(-0.05 * (Math.pow(x1s - 0.3223304,2) + Math.pow(x2s - 1.132523,2) + Math.pow(x3s - 0.6979448,2) + Math.pow(x4s - 0.2356416,2) + Math.pow(x5s - -0.1445148,2) + Math.pow(x6s - 0.3877619,2) + Math.pow(x7s - -1.003584,2))) //index=33;
    z = z + 32 * Math.exp(-0.05 * (Math.pow(x1s - 0.3223304,2) + Math.pow(x2s - 1.132523,2) + Math.pow(x3s - -0.9825947,2) + Math.pow(x4s - -0.7085256,2) + Math.pow(x5s - -0.1445148,2) + Math.pow(x6s - 0.3877619,2) + Math.pow(x7s - -1.003584,2))) //index=34;
    z = z + -2.29071 * Math.exp(-0.05 * (Math.pow(x1s - 0.3223304,2) + Math.pow(x2s - 0.5882506,2) + Math.pow(x3s - -0.7317679,2) + Math.pow(x4s - -0.5676051,2) + Math.pow(x5s - -0.1445148,2) + Math.pow(x6s - 0.3877619,2) + Math.pow(x7s - -0.7456196,2))) //index=35;
    z = z + 32 * Math.exp(-0.05 * (Math.pow(x1s - 0.3223304,2) + Math.pow(x2s - 0.5882506,2) + Math.pow(x3s - -0.9825947,2) + Math.pow(x4s - -0.7085256,2) + Math.pow(x5s - -0.1445148,2) + Math.pow(x6s - 0.3877619,2) + Math.pow(x7s - -0.7456196,2))) //index=36;
    z = z + 26.93546 * Math.exp(-0.05 * (Math.pow(x1s - 0.3223304,2) + Math.pow(x2s - 0.04397874,2) + Math.pow(x3s - -1.735075,2) + Math.pow(x4s - -0.7085256,2) + Math.pow(x5s - 0.2486783,2) + Math.pow(x6s - 0.3877619,2) + Math.pow(x7s - -0.3469475,2))) //index=37;
    z = z + 32 * Math.exp(-0.05 * (Math.pow(x1s - 0.3223304,2) + Math.pow(x2s - 0.04397874,2) + Math.pow(x3s - -0.05453558,2) + Math.pow(x4s - -0.1871198,2) + Math.pow(x5s - -0.1445148,2) + Math.pow(x6s - 0.3877619,2) + Math.pow(x7s - -0.3469475,2))) //index=38;
    z = z + -32 * Math.exp(-0.05 * (Math.pow(x1s - 0.3223304,2) + Math.pow(x2s - 0.04397874,2) + Math.pow(x3s - -0.7317679,2) + Math.pow(x4s - -0.5676051,2) + Math.pow(x5s - -0.1445148,2) + Math.pow(x6s - 0.3877619,2) + Math.pow(x7s - -0.3469475,2))) //index=39;
    z = z + -32 * Math.exp(-0.05 * (Math.pow(x1s - 0.3223304,2) + Math.pow(x2s - 0.04397874,2) + Math.pow(x3s - -0.9825947,2) + Math.pow(x4s - -0.7085256,2) + Math.pow(x5s - -0.1445148,2) + Math.pow(x6s - 0.3877619,2) + Math.pow(x7s - -0.3469475,2))) //index=40;
    z = z + 32 * Math.exp(-0.05 * (Math.pow(x1s - 0.3223304,2) + Math.pow(x2s - 0.04397874,2) + Math.pow(x3s - 0.02071246,2) + Math.pow(x4s - -0.5676051,2) + Math.pow(x5s - -0.5377078,2) + Math.pow(x6s - 0.3877619,2) + Math.pow(x7s - -0.3469475,2))) //index=41;
    z = z + 32 * Math.exp(-0.05 * (Math.pow(x1s - 0.9605058,2) + Math.pow(x2s - 0.7394373,2) + Math.pow(x3s - 0.4666268,2) + Math.pow(x4s - -0.2544485,2) + Math.pow(x5s - -0.479457,2) + Math.pow(x6s - -0.3357828,2) + Math.pow(x7s - -0.8392403,2))) //index=42;
    z = z + -30.49333 * Math.exp(-0.05 * (Math.pow(x1s - 0.9605058,2) + Math.pow(x2s - 0.7394373,2) + Math.pow(x3s - -0.3415929,2) + Math.pow(x4s - -0.7085256,2) + Math.pow(x5s - -0.479457,2) + Math.pow(x6s - -0.3357828,2) + Math.pow(x7s - -0.8392403,2))) //index=43;
    z = z + -32 * Math.exp(-0.05 * (Math.pow(x1s - 0.3223304,2) + Math.pow(x2s - -0.5002931,2) + Math.pow(x3s - 0.6728621,2) + Math.pow(x4s - 0.2215495,2) + Math.pow(x5s - -0.1445148,2) + Math.pow(x6s - 0.3877619,2) + Math.pow(x7s - 0.3507286,2))) //index=44;
    z = z + -32 * Math.exp(-0.05 * (Math.pow(x1s - 0.3223304,2) + Math.pow(x2s - -0.5002931,2) + Math.pow(x3s - -0.9825947,2) + Math.pow(x4s - -0.7085256,2) + Math.pow(x5s - -0.1445148,2) + Math.pow(x6s - 0.3877619,2) + Math.pow(x7s - 0.3507286,2))) //index=46;
    z = z + 13.18468 * Math.exp(-0.05 * (Math.pow(x1s - -0.8802637,2) + Math.pow(x2s - -0.750364,2) + Math.pow(x3s - 1.224681,2) + Math.pow(x4s - -0.03210731,2) + Math.pow(x5s - -0.6687722,2) + Math.pow(x6s - 1.429666,2) + Math.pow(x7s - 0.9041234,2))) //index=47;
    z = z + 24.50846 * Math.exp(-0.05 * (Math.pow(x1s - -0.8802637,2) + Math.pow(x2s - -0.750364,2) + Math.pow(x3s - 0.02071246,2) + Math.pow(x4s - -0.7085256,2) + Math.pow(x5s - -0.6687722,2) + Math.pow(x6s - 1.429666,2) + Math.pow(x7s - 0.9041234,2))) //index=48;
    z = z + 32 * Math.exp(-0.05 * (Math.pow(x1s - 1.679275,2) + Math.pow(x2s - 1.676794,2) + Math.pow(x3s - 0.9236889,2) + Math.pow(x4s - 2.138068,2) + Math.pow(x5s - 1.480683,2) + Math.pow(x6s - -0.7192615,2) + Math.pow(x7s - -0.7665024,2))) //index=49;
    z = z + 32 * Math.exp(-0.05 * (Math.pow(x1s - 1.679275,2) + Math.pow(x2s - 1.676794,2) + Math.pow(x3s - 0.02071246,2) + Math.pow(x4s - 2.138068,2) + Math.pow(x5s - 1.952515,2) + Math.pow(x6s - -0.7192615,2) + Math.pow(x7s - -0.7665024,2))) //index=50;
    z = z + 0.5398509 * Math.exp(-0.05 * (Math.pow(x1s - 1.679275,2) + Math.pow(x2s - 1.676794,2) + Math.pow(x3s - -1.233422,2) + Math.pow(x4s - 2.138068,2) + Math.pow(x5s - 2.607837,2) + Math.pow(x6s - -0.7192615,2) + Math.pow(x7s - -0.7665024,2))) //index=51;
    z = z + -32 * Math.exp(-0.05 * (Math.pow(x1s - -1.222168,2) + Math.pow(x2s - -0.1374452,2) + Math.pow(x3s - -0.6343965,2) + Math.pow(x4s - -0.7085256,2) + Math.pow(x5s - -0.3264585,2) + Math.pow(x6s - 0.6954922,2) + Math.pow(x7s - -0.1609005,2))) //index=52;
    z = z + -32 * Math.exp(-0.05 * (Math.pow(x1s - -1.222168,2) + Math.pow(x2s - -0.1374452,2) + Math.pow(x3s - 0.8060113,2) + Math.pow(x4s - 0.08168268,2) + Math.pow(x5s - -0.3441735,2) + Math.pow(x6s - 0.6514838,2) + Math.pow(x7s - -0.1609005,2))) //index=53;
    z = z + -32 * Math.exp(-0.05 * (Math.pow(x1s - -1.222168,2) + Math.pow(x2s - -0.1374452,2) + Math.pow(x3s - 0.2715393,2) + Math.pow(x4s - -0.7085256,2) + Math.pow(x5s - -0.7998365,2) + Math.pow(x6s - -0.4804917,2) + Math.pow(x7s - -0.1609005,2))) //index=54;
    z = z + -32 * Math.exp(-0.05 * (Math.pow(x1s - -1.222168,2) + Math.pow(x2s - -0.1374452,2) + Math.pow(x3s - -2.441106,2) + Math.pow(x4s - -0.7085256,2) + Math.pow(x5s - 0.6176002,2) + Math.pow(x6s - 3.040759,2) + Math.pow(x7s - -0.1609005,2))) //index=55;
    z = z + -3.530533 * Math.exp(-0.05 * (Math.pow(x1s - -1.434358,2) + Math.pow(x2s - -0.1374452,2) + Math.pow(x3s - -2.738382,2) + Math.pow(x4s - -0.7085256,2) + Math.pow(x5s - 0.7729357,2) + Math.pow(x6s - 3.42665,2) + Math.pow(x7s - -0.1609005,2))) //index=56;
    z = z + 3.612274 * Math.exp(-0.05 * (Math.pow(x1s - -1.434358,2) + Math.pow(x2s - -0.1374452,2) + Math.pow(x3s - -2.373543,2) + Math.pow(x4s - -0.7085256,2) + Math.pow(x5s - 0.5822966,2) + Math.pow(x6s - 2.953057,2) + Math.pow(x7s - -0.1609005,2))) //index=57;
    z = z + -32 * Math.exp(-0.05 * (Math.pow(x1s - -1.434358,2) + Math.pow(x2s - -0.1374452,2) + Math.pow(x3s - -1.400639,2) + Math.pow(x4s - -0.7085256,2) + Math.pow(x5s - 0.07392583,2) + Math.pow(x6s - 1.690142,2) + Math.pow(x7s - -0.1609005,2))) //index=58;
    z = z + 32 * Math.exp(-0.05 * (Math.pow(x1s - -1.434358,2) + Math.pow(x2s - -0.1374452,2) + Math.pow(x3s - -0.6362149,2) + Math.pow(x4s - -0.7085256,2) + Math.pow(x5s - -0.3255084,2) + Math.pow(x6s - 0.6978525,2) + Math.pow(x7s - -0.1609005,2))) //index=59;
    z = z + 32 * Math.exp(-0.05 * (Math.pow(x1s - -1.434358,2) + Math.pow(x2s - -0.1374452,2) + Math.pow(x3s - -2.373543,2) + Math.pow(x4s - -0.7085256,2) + Math.pow(x5s - 0.5822966,2) + Math.pow(x6s - 2.953057,2) + Math.pow(x7s - -0.1609005,2))) //index=60;
    z = z + -26.11138 * Math.exp(-0.05 * (Math.pow(x1s - 1.381192,2) + Math.pow(x2s - 1.676794,2) + Math.pow(x3s - 1.255096,2) + Math.pow(x4s - 2.109884,2) + Math.pow(x5s - 1.307513,2) + Math.pow(x6s - -0.7573758,2) + Math.pow(x7s - -1.184159,2))) //index=61;
    z = z + -7.346812 * Math.exp(-0.05 * (Math.pow(x1s - 1.381192,2) + Math.pow(x2s - 1.676794,2) + Math.pow(x3s - 0.2873394,2) + Math.pow(x4s - 2.109884,2) + Math.pow(x5s - 1.813194,2) + Math.pow(x6s - -0.7573758,2) + Math.pow(x7s - -1.184159,2))) //index=64;
    z = z + -32 * Math.exp(-0.05 * (Math.pow(x1s - 1.381192,2) + Math.pow(x2s - 1.676794,2) + Math.pow(x3s - 0.08983796,2) + Math.pow(x4s - 2.109884,2) + Math.pow(x5s - 1.916395,2) + Math.pow(x6s - -0.7573758,2) + Math.pow(x7s - -1.184159,2))) //index=65;
    z = z + -4.084295 * Math.exp(-0.05 * (Math.pow(x1s - 1.381192,2) + Math.pow(x2s - 1.676794,2) + Math.pow(x3s - -0.7001677,2) + Math.pow(x4s - 2.109884,2) + Math.pow(x5s - 2.329196,2) + Math.pow(x6s - -0.7573758,2) + Math.pow(x7s - -1.184159,2))) //index=69;
    z = z + -0.3761257 * Math.exp(-0.05 * (Math.pow(x1s - 1.381192,2) + Math.pow(x2s - 1.676794,2) + Math.pow(x3s - -1.687675,2) + Math.pow(x4s - 2.109884,2) + Math.pow(x5s - 2.845197,2) + Math.pow(x6s - -0.7573758,2) + Math.pow(x7s - -1.184159,2))) //index=70;
    z = z + -12.03388 * Math.exp(-0.05 * (Math.pow(x1s - -1.331298,2) + Math.pow(x2s - -0.04195893,2) + Math.pow(x3s - 1.266888,2) + Math.pow(x4s - -0.0297458,2) + Math.pow(x5s - -0.6886304,2) + Math.pow(x6s - -0.5331132,2) + Math.pow(x7s - -0.3655522,2))) //index=71;
    z = z + 18.59006 * Math.exp(-0.05 * (Math.pow(x1s - -0.5066796,2) + Math.pow(x2s - -1.044565,2) + Math.pow(x3s - 1.136753,2) + Math.pow(x4s - -0.5042688,2) + Math.pow(x5s - -1.061965,2) + Math.pow(x6s - -1.348745,2) + Math.pow(x7s - -1.184159,2))) //index=72;
    z = z + 3.659309 * Math.exp(-0.05 * (Math.pow(x1s - -0.5066796,2) + Math.pow(x2s - -1.044565,2) + Math.pow(x3s - 0.7731928,2) + Math.pow(x4s - -0.7085256,2) + Math.pow(x5s - -1.061965,2) + Math.pow(x6s - -1.348745,2) + Math.pow(x7s - -1.184159,2))) //index=73;
    z = z + 31.4439 * Math.exp(-0.05 * (Math.pow(x1s - -1.434358,2) + Math.pow(x2s - -0.7905715,2) + Math.pow(x3s - -1.233422,2) + Math.pow(x4s - -0.7085256,2) + Math.pow(x5s - -0.01345041,2) + Math.pow(x6s - 1.473079,2) + Math.pow(x7s - 0.9902651,2))) //index=74;
    z = z + 32 * Math.exp(-0.05 * (Math.pow(x1s - -0.3625288,2) + Math.pow(x2s - -1.044565,2) + Math.pow(x3s - 0.6728621,2) + Math.pow(x4s - -0.3703164,2) + Math.pow(x5s - -0.694985,2) + Math.pow(x6s - -0.7409679,2) + Math.pow(x7s - 0.5894889,2))) //index=75;
    z = z + 32 * Math.exp(-0.05 * (Math.pow(x1s - -0.3625288,2) + Math.pow(x2s - -1.044565,2) + Math.pow(x3s - 0.07087782,2) + Math.pow(x4s - -0.7085256,2) + Math.pow(x5s - -0.694985,2) + Math.pow(x6s - -0.7409679,2) + Math.pow(x7s - 0.5894889,2))) //index=76;
    z = z + 1.554901 * Math.exp(-0.05 * (Math.pow(x1s - 2.459456,2) + Math.pow(x2s - 0.2254027,2) + Math.pow(x3s - 0.6728621,2) + Math.pow(x4s - -0.3703164,2) + Math.pow(x5s - -0.694985,2) + Math.pow(x6s - -1.348745,2) + Math.pow(x7s - -0.5019866,2))) //index=77;
    //////////////////////////////////////////////////////////////////////////////;
    z = z * 0.343375980384612 + 0.768381801818584;
  
 
  return z;
}

function SVMc5(x1, x2, x3, x4, x5, x6, x7){
  var x1s, x2s, x3s, x4s, x5s, x6s, x7s, z ;
  x2s = (x2 - 0.461795832688657) / 0.204196476765282;
    x4s = (x4 - -0.363639529024974) / 0.343545300379106;
    x5s = (x5 - 0.236624412784027) / 0.185741967040501;
    x6s = (x6 - 0.904740994580395) / 0.45045720266353;
    x7s = (x7 - 0.757883138582498) / 0.46182172012942;
     ////////////////////////////////////////////////////////////;
    z = +0.3307852;
    z = z + 8.828453 * Math.exp(-0.1 * (Math.pow(x2s - -1.064423,2) + Math.pow(x4s - -0.02775182,2) + Math.pow(x5s - -1.161687,2) + Math.pow(x6s - -0.5174947,2) + Math.pow(x7s - -0.1647063,2))) //index=1;
    z = z + -16 * Math.exp(-0.1 * (Math.pow(x2s - -1.064423,2) + Math.pow(x4s - 0.9818125,2) + Math.pow(x5s - -0.1404924,2) + Math.pow(x6s - -0.6940236,2) + Math.pow(x7s - -0.1647063,2))) //index=2;
    z = z + -16 * Math.exp(-0.1 * (Math.pow(x2s - 0.02385366,2) + Math.pow(x4s - -0.6766682,2) + Math.pow(x5s - -0.8890626,2) + Math.pow(x6s - -0.6852483,2) + Math.pow(x7s - -0.3521815,2))) //index=3;
    z = z + 16 * Math.exp(-0.1 * (Math.pow(x2s - 0.02385366,2) + Math.pow(x4s - 0.8477596,2) + Math.pow(x5s - 0.2137105,2) + Math.pow(x6s - -0.6940236,2) + Math.pow(x7s - -0.3521815,2))) //index=4;
    z = z + -16 * Math.exp(-0.1 * (Math.pow(x2s - 0.02385366,2) + Math.pow(x4s - 0.2721387,2) + Math.pow(x5s - 0.2538146,2) + Math.pow(x6s - -0.6585881,2) + Math.pow(x7s - -0.3521815,2))) //index=5;
    z = z + -2.305833 * Math.exp(-0.1 * (Math.pow(x2s - 0.02385366,2) + Math.pow(x4s - 0.5964016,2) + Math.pow(x5s - 0.2856798,2) + Math.pow(x6s - -0.6304325,2) + Math.pow(x7s - -0.3521815,2))) //index=6;
    z = z + 6.576594 * Math.exp(-0.1 * (Math.pow(x2s - -1.390906,2) + Math.pow(x4s - 0.4163972,2) + Math.pow(x5s - -0.9414119,2) + Math.pow(x6s - -0.5393993,2) + Math.pow(x7s - -0.01706966,2))) //index=7;
    z = z + -16 * Math.exp(-0.1 * (Math.pow(x2s - -1.390906,2) + Math.pow(x4s - 0.6842419,2) + Math.pow(x5s - -0.4894431,2) + Math.pow(x6s - -0.5813734,2) + Math.pow(x7s - 0.3889312,2))) //index=9;
    z = z + 1.970471 * Math.exp(-0.1 * (Math.pow(x2s - -0.6401421,2) + Math.pow(x4s - 0.8547331,2) + Math.pow(x5s - 1.417965,2) + Math.pow(x6s - 1.277056,2) + Math.pow(x7s - 0.5905506,2))) //index=10;
    z = z + -16 * Math.exp(-0.1 * (Math.pow(x2s - -0.6401421,2) + Math.pow(x4s - 0.6800836,2) + Math.pow(x5s - 1.417965,2) + Math.pow(x6s - 1.277056,2) + Math.pow(x7s - 0.5905506,2))) //index=11;
    z = z + 0.1638964 * Math.exp(-0.1 * (Math.pow(x2s - -0.6401421,2) + Math.pow(x4s - -2.143416,2) + Math.pow(x5s - -0.8647718,2) + Math.pow(x6s - 1.277056,2) + Math.pow(x7s - 0.5905506,2))) //index=12;
    z = z + 7.50387 * Math.exp(-0.1 * (Math.pow(x2s - -0.6401421,2) + Math.pow(x4s - 0.6509754,2) + Math.pow(x5s - -1.220103,2) + Math.pow(x6s - 1.277056,2) + Math.pow(x7s - 0.5905506,2))) //index=13;
    z = z + -16 * Math.exp(-0.1 * (Math.pow(x2s - -0.6401421,2) + Math.pow(x4s - -0.3969214,2) + Math.pow(x5s - -1.220103,2) + Math.pow(x6s - 1.277056,2) + Math.pow(x7s - 0.5905506,2))) //index=14;
    z = z + 2.871868 * Math.exp(-0.1 * (Math.pow(x2s - 1.656268,2) + Math.pow(x4s - -2.58004,2) + Math.pow(x5s - -0.600965,2) + Math.pow(x6s - -0.1585226,2) + Math.pow(x7s - -1.370406,2))) //index=15;
    z = z + 16 * Math.exp(-0.1 * (Math.pow(x2s - 1.656268,2) + Math.pow(x4s - 0.3792984,2) + Math.pow(x5s - -0.600965,2) + Math.pow(x6s - -0.1585226,2) + Math.pow(x7s - -1.370406,2))) //index=16;
    z = z + -12.37788 * Math.exp(-0.1 * (Math.pow(x2s - 1.656268,2) + Math.pow(x4s - 0.8037936,2) + Math.pow(x5s - -0.600965,2) + Math.pow(x6s - -0.1585226,2) + Math.pow(x7s - -1.370406,2))) //index=17;
    z = z + -12.56668 * Math.exp(-0.1 * (Math.pow(x2s - 1.329785,2) + Math.pow(x4s - -0.03306833,2) + Math.pow(x5s - -0.600965,2) + Math.pow(x6s - -0.6210157,2) + Math.pow(x7s - -1.247375,2))) //index=19;
    z = z + 16 * Math.exp(-0.1 * (Math.pow(x2s - 1.329785,2) + Math.pow(x4s - 0.5248397,2) + Math.pow(x5s - -0.600965,2) + Math.pow(x6s - -0.6210157,2) + Math.pow(x7s - -1.247375,2))) //index=20;
    z = z + -9.937267 * Math.exp(-0.1 * (Math.pow(x2s - 1.329785,2) + Math.pow(x4s - 0.9614633,2) + Math.pow(x5s - -0.600965,2) + Math.pow(x6s - -0.6210157,2) + Math.pow(x7s - -1.247375,2))) //index=21;
    z = z + 8.938678 * Math.exp(-0.1 * (Math.pow(x2s - -0.4077679,2) + Math.pow(x4s - -0.6394901,2) + Math.pow(x5s - -0.8073444,2) + Math.pow(x6s - -0.3712694,2) + Math.pow(x7s - 1.396864,2))) //index=22;
    z = z + -7.577367 * Math.exp(-0.1 * (Math.pow(x2s - -0.4077679,2) + Math.pow(x4s - 0.9372064,2) + Math.pow(x5s - 1.216072,2) + Math.pow(x6s - -0.3712694,2) + Math.pow(x7s - 1.396864,2))) //index=24;
    z = z + -13.48805 * Math.exp(-0.1 * (Math.pow(x2s - -0.3535097,2) + Math.pow(x4s - 0.4035553,2) + Math.pow(x5s - -0.8701556,2) + Math.pow(x6s - -1.296256,2) + Math.pow(x7s - 0.2355531,2))) //index=25;
    z = z + 16 * Math.exp(-0.1 * (Math.pow(x2s - -0.3535097,2) + Math.pow(x4s - 0.8886927,2) + Math.pow(x5s - -0.4888022,2) + Math.pow(x6s - -1.296256,2) + Math.pow(x7s - 0.2355531,2))) //index=27;
    z = z + -3.028751 * Math.exp(-0.1 * (Math.pow(x2s - 0.1273726,2) + Math.pow(x4s - -0.5182057,2) + Math.pow(x5s - -0.9598858,2) + Math.pow(x6s - -0.870762,2) + Math.pow(x7s - 2.328713,2))) //index=28;
    z = z + 1.141018 * Math.exp(-0.1 * (Math.pow(x2s - 0.1273726,2) + Math.pow(x4s - 0.08821601,2) + Math.pow(x5s - 0.1617419,2) + Math.pow(x6s - -0.870762,2) + Math.pow(x7s - 2.328713,2))) //index=29;
    z = z + 0.9339773 * Math.exp(-0.1 * (Math.pow(x2s - 1.901131,2) + Math.pow(x4s - -0.3969214,2) + Math.pow(x5s - 1.956346,2) + Math.pow(x6s - 0.2114718,2) + Math.pow(x7s - -1.450014,2))) //index=30;
    z = z + -0.7185166 * Math.exp(-0.1 * (Math.pow(x2s - 1.901131,2) + Math.pow(x4s - -3.307746,2) + Math.pow(x5s - 1.633317,2) + Math.pow(x6s - 0.2114718,2) + Math.pow(x7s - -1.450014,2))) //index=31;
    z = z + -0.4917741 * Math.exp(-0.1 * (Math.pow(x2s - 1.901131,2) + Math.pow(x4s - 0.3744471,2) + Math.pow(x5s - 1.148774,2) + Math.pow(x6s - 0.2114718,2) + Math.pow(x7s - -1.450014,2))) //index=32;
    z = z + -16 * Math.exp(-0.1 * (Math.pow(x2s - -0.6401421,2) + Math.pow(x4s - 0.3307847,2) + Math.pow(x5s - 1.417965,2) + Math.pow(x6s - 1.277056,2) + Math.pow(x7s - 0.5684553,2))) //index=33;
    z = z + 16 * Math.exp(-0.1 * (Math.pow(x2s - -0.6401421,2) + Math.pow(x4s - 0.8256248,2) + Math.pow(x5s - 0.07201166,2) + Math.pow(x6s - 1.277056,2) + Math.pow(x7s - 0.5684553,2))) //index=34;
    z = z + -16 * Math.exp(-0.1 * (Math.pow(x2s - -0.6401421,2) + Math.pow(x4s - 0.3307847,2) + Math.pow(x5s - 0.07201166,2) + Math.pow(x6s - 1.277056,2) + Math.pow(x7s - 0.5684553,2))) //index=35;
    z = z + -6.646445 * Math.exp(-0.1 * (Math.pow(x2s - 0.1870951,2) + Math.pow(x4s - -0.7063778,2) + Math.pow(x5s - 2.303343,2) + Math.pow(x6s - 2.416688,2) + Math.pow(x7s - -0.5584041,2))) //index=36;
    z = z + 14.75697 * Math.exp(-0.1 * (Math.pow(x2s - 0.31597,2) + Math.pow(x4s - -0.2764176,2) + Math.pow(x5s - 0.0312252,2) + Math.pow(x6s - -0.7303322,2) + Math.pow(x7s - -0.7749379,2))) //index=37;
    z = z + 6.097774 * Math.exp(-0.1 * (Math.pow(x2s - -0.6944088,2) + Math.pow(x4s - -1.881442,2) + Math.pow(x5s - -0.5094401,2) + Math.pow(x6s - 1.321455,2) + Math.pow(x7s - 0.6595984,2))) //index=38;
    z = z + 9.046275 * Math.exp(-0.1 * (Math.pow(x2s - -0.6944088,2) + Math.pow(x4s - -0.4551379,2) + Math.pow(x5s - 1.417965,2) + Math.pow(x6s - 1.321455,2) + Math.pow(x7s - 0.6595984,2))) //index=39;
    z = z + 16 * Math.exp(-0.1 * (Math.pow(x2s - -0.6944088,2) + Math.pow(x4s - 0.3307847,2) + Math.pow(x5s - 1.417965,2) + Math.pow(x6s - 1.321455,2) + Math.pow(x7s - 0.6595984,2))) //index=40;
    z = z + 16 * Math.exp(-0.1 * (Math.pow(x2s - -0.6944088,2) + Math.pow(x4s - 0.6509754,2) + Math.pow(x5s - 1.417965,2) + Math.pow(x6s - 1.321455,2) + Math.pow(x7s - 0.6595984,2))) //index=41;
    z = z + 16 * Math.exp(-0.1 * (Math.pow(x2s - -1.037216,2) + Math.pow(x4s - 0.3598929,2) + Math.pow(x5s - -1.015519,2) + Math.pow(x6s - -0.9429109,2) + Math.pow(x7s - 0.2355531,2))) //index=42;
    z = z + 0.3087149 * Math.exp(-0.1 * (Math.pow(x2s - 0.6768195,2) + Math.pow(x4s - 0.3598929,2) + Math.pow(x5s - -1.015519,2) + Math.pow(x6s - -1.564502,2) + Math.pow(x7s - -0.9192937,2))) //index=43;
    //////////////////////////////////////////////////////////////////////////////;
    z = z * 0.246125768583108 + 0.74638369329017;
    
 
  return z;
}
