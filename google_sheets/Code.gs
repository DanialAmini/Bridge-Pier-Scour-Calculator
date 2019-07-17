//google sheets script

function myFunction(a,b) {
  var new_;
  new_=Math.pow(a,b);
  return new_+myFunction2(a,b);
}

//
//function pow(a,b) {
//  var temp;
//  temp = Math.pow(a,b);
//  return temp;
//}

function caseFinder(type_, h0, h1, y, bcol) {
  var z=0;
  if (type_ == "complex pier") {
    if (h0 >= y){
      z = 1;
    } else if (h1 >= y && h0 < y && h0 > 0){
      z = 2;
    } else if (h1 < y && h0 >= 0) {
      z = 3;
    } else if (h1 >= 0 && h0 < 0) {
      z = 4;
    } else if (h1 < 0 && h1 > -2.4 * bcol) {
      z = 5;
    } else if (h1 <= -2.4 * bcol) {
      z = 6;
    } else {
      z = 0;
    }
  }
  
  return z;
}

function log(x){
  return Math.log(x)/Math.log(10);
}

function ln(x){
  return Math.log(x);
}

function bstar3(h0, h1, y, ysHEC, bcol, bpc, bpg, Kscol, Kspc, Kspg){
  var hp0, hp1;
  hp0 = h0 ; hp1 = h1;
  if (h0 > y) { hp0 = y;}
  if (h1 > y ){ hp1 = y;}
  if (h0 < -ysHEC ){ hp0 = -ysHEC;}
  if (h1 < -ysHEC ){ hp1 = -ysHEC;}

  return (y - hp1) / (y + ysHEC) * Kscol * bcol + (hp1 - hp0) / (y + ysHEC) * Kspc * bpc + (hp0 + ysHEC) / (y + ysHEC) * Kspg * bpg;
}

function bpgHEC(bp, n, m, Sn, Sm){
  var Km, Ksp, bproj ;
  Km = 0 ; Ksp = 0 ; bproj = n * bp;
  
  if (bp == 0 ){
    //            'nothing
  }else{
    if (m == 1 ){
      Km = 1;
    }else{
      Km = 0.9 + 0.1 * Math.min(m, 6) - 0.0714 * (Math.min(m, 6) - 1) * (2.4 - 1.1 * (Sm / bp) + 0.1 * Math.pow(Sm / bp, 2));
    }
    
    if (n == 1 ){
      Ksp = 1;
    }else{
      Ksp = 1 - 4 / 3 * (1 - 1 / (bproj / bp)) * (1 - Math.pow(Sn / bp, -0.6));
    }
  }
  
  return bproj * Km * Ksp;
}

function hecCalc(type_ , d50 , sigma_g , y , V , bcol , Kscol , bpc , T , f1 , h0 , h1 , Kspc , bp , m , n , Sn , Sm , Kspg ) {
  
  var i;
  var case___  =0 ;
  var case_hec  =0 ;
  var y2, V2, h2, yf ;
  var k2  = 1; //angle of attack
  var k3  = 1.1; //bed condition
  var g  = 9.81;
  var ustarc, Vc ;
  var yscol, yscol_alone, f1coef, h1coef, Khpier ;
  var Tp, y2max, bstarpc, h2y2, y2new, Kw1, yspc1 ;
  var yspc2, Vf, ks, Kw2 ;
  var yspg1  = 0;
  var ys1, ys2, ys ;
  var y3, V3, h3 ;
  var Ksp, Km, Khpg, bproj, bpg ;
  var y3max, y3new, h3y3, hec_calc_ ;
  
  var output_=[26];
  
  for (i = 0;i<=26;i++){
    output_[i] = 0;
  }
  
  case_ = caseFinder(type_, h0, h1, y, bcol);
  output_[0] = case_;
  
  
  if (d50 < 1){
    ustarc = 0.3048 * (0.0377 + 0.041 * Math.pow(d50 , 1.4));
  } else if (d50 < 100) {
    ustarc = 0.3048 * (0.1 * Math.pow(d50 , 0.5) - 0.0213 / d50);
  }
  
  output_[1] = ustarc;
  
  Vc = 5.75 * ustarc * Math.log(5.53 * y / (d50 / 1000))/Math.log(10);
 
  output_[2] = Vc;
  
  if ( case_ >= 3 && case_ <= 6 && bcol > 0 && h1 < y ){
    
    yscol_alone = y * 2.0 * Kscol * k2 * k3 * Math.pow(bcol / y, 0.65) * Math.pow(V / Math.sqrt(g * y),0.43);
    output_[3] = yscol_alone;
    
    if ( h1 < -yscol_alone ){
      Khpier = 1;
    } else { 
      f1coef = Math.max(Math.min(f1 / bcol, 1.5), 0);
      h1coef = Math.max(Math.min(h1 / bcol, 1.6), -0.8);
      Khpier = (0.4075 - 0.0669 * f1coef) - (0.4271 - 0.0778 * f1coef) * h1coef + (0.1615 - 0.0455 * f1coef) * Math.pow(h1coef , 2) - (0.0269 - 0.012 * f1coef) * Math.pow(h1coef ,3);
    }
    output_[4] = Khpier;
    
    yscol = y * Khpier * 2.0 * Kscol * k2 * k3 * Math.pow(bcol / y, 0.65) * Math.pow(V / Math.sqrt(g * y), 0.43);
  } else {
    yscol = 0;
  }
  
  //column
  
  output_[5] = yscol;
  
  y2 = y + yscol / 2;
  h2 = h0 + yscol / 2 ;
  V2 = V * y / y2 ;
  yf = h1 + yscol / 2;
  
  output_[6]= y2;
  output_[7] = h2;
  output_[8] = V2;
  output_[9] = yf;
  
  if ( h2 < 0 ) {
    case_hec = 2;
  } else if ( h2 > 0 ) {
    case_hec = 1;
  } else if ( h2 == 0 ) {
    case_hec = 12; //between case1&2?
  }
  
  output_[10]= case_hec;
  
 
  if ( case_hec == 1 || case_hec == 12 ) {
    if ( h0 >= y || T == 0 || bpc == 0 ) {
      yspc1 = 0;
    } else {
      Tp = Math.min(y2, yf) - h2;
      y2max = 3.5 * bpc;
      y2new = Math.min(y2, y2max);
      h2y2 = Math.min(Math.max(h2 / y2new, 0), 1);
      if ( Tp == 0 ) { bstarpc = 0; } 
      bstarpc = bpc * Math.exp(-2.705 + 0.51 * Math.log(Tp / y2new) - 2.783 * Math.pow(h2y2 , 3) + 1.751 / Math.exp(h2y2));
    } 
    output_[11] = bstarpc ;
            
    if ( bpc == 0 || bstarpc == 0 ) { Kw1 = 0; } 
    if ( y2 < 0.8 * bstarpc && V2 / Math.sqrt(g * y2) < 1 && bstarpc > 50 * d50 / 1000 ) {
      if ( V / Vc < 1 ) {
        Kw1 = 2.58 * Math.pow(y2 / bpc, 0.34) * Math.pow(V2 / Math.sqrt(g * y2), 0.65);
      } else if ( V / Vc >= 1 ) {
        Kw1 = 1.0 * Math.pow(y2 / bpc, 0.13) * Math.pow(V2 / Math.sqrt(g * y2), 0.25);
      } else {
        Kw1 = 0;
      } 
    } else { 
      Kw1 = 1.0
    } 
    
    output_[12] = Kw1;
            
    if ( h0 >= y || T == 0 || Tp == 0 || bstarpc == 0 ) { 
      yspc1 = 0 ;
    } else {
      yspc1 = y2new * 2.0 * Kspc * k2 * k3 * Kw1 * Math.pow(bstarpc / y2new, 0.65) * Math.pow(V2 / Math.sqrt(g * y2new), 0.43);
    } 
    output_[13] = yspc1;
            
            
    y3 = y + yscol / 2 + yspc1 / 2;
    V3 = V * y / y3;
    h3 = h0 + yscol / 2 + yspc1 / 2;
    output_[14] = y3;
    output_[15] = V3;
    output_[16] = h3;
    
    if ( bp > 0 && h3 > 0 ) { bproj = n * bp;}
    
    if ( m == 1 ) { 
      Km = 1;
    } else {
      Km = 0.9 + 0.1 * Math.min(m, 6) - 0.0714 * (Math.min(m, 6) - 1) * (2.4 - 1.1 * (Sm / bp) + 0.1 * Math.pow(Sm / bp, 2));
    } 
    output_[17] = Km
            
    if ( n == 1 ) {
      Ksp = 1;
    } else {
      Ksp = 1 - 4 / 3 * (1 - 1 / (bproj / bp)) * (1 - Math.pow(Sn / bp, -0.6));
    } 
    output_[18] = Ksp
            
    bpg = bproj * Km * Ksp;
    y3max = 3.5 * bpg;
    y3new = Math.min(y3, y3max)
    h3y3 = Math.min(Math.max(h3 / y3new, 0), 1)
    Khpg = Math.pow(3.08 * h3y3 - 5.23 * Math.pow(h3y3 , 2) + 5.25 * Math.pow(h3y3 , 3) - 2.1 * Math.pow(h3y3 , 4), (1 / 0.65));
    yspg1 = y3new * Khpg * 2.0 * Kspg * k3 * Math.pow(bpg / y3new, 0.65) * Math.pow(V3 / Math.sqrt(g * y3new),  0.43);
    output_[19] = Khpg;
            
  } else {
    yspg1 = 0;
  }
  output_[20] = yspg1;
  ys1 = yscol + yspc1 + yspg1;
  output_[21] = ys1;
  
  if ( case_hec == 2 || case_hec == 12 ) {

    if ( bpc == 0 || yf <= 0 ) {  
      yspc2 = 0; 
    } else {
      if ( d50 < 2 ) { 
        ks = d50 / 1000;
      } else if ( d50 > 2 ) { 
        ks = d50 * 3.5 / 1000;
      } 
      if ( sigma_g > 0 ) {
        ks = ks * sigma_g;
      } else {
        ks = ks * 1.3;
      } 
      
      Vf = V2 * Math.log(10.93 * yf / ks + 1) / Math.log(10.93 * y2 / ks + 1);
      output_[22] = Vf;
      
      if ( bpc > 0 ) {
        if ( y2 < 0.8 * bpc && V2 / Math.sqrt(g * y2) < 1 && bpc > 50 * d50 / 1000 ) {
          if ( V / Vc < 1 ) {
            Kw2 = 2.58 * Math.pow(y2 / bpc, 0.34) * Math.pow(V2 / Math.sqrt(g * y2), 0.65);
          } else if ( V / Vc >= 1 ) {
            Kw2 = 1.0 * Math.pow(y2 / bpc, 0.13) * Math.pow(V2 / Math.sqrt(g * y2),0.25);
          } else {
            Kw2 = 0;
          } 
        } else{
          Kw2 = 1.0;
        } 
      } else {
        Kw2 = 0;
      } 
      output_[23] = Kw2;
      
      yspc2 = yf * 2.0 * Kspc * k2 * k3 * Kw2 * Math.pow(bpc / yf,0.65) * Math.pow(Vf / Math.sqrt(g * yf),0.43);
    } 
  }
  
  output_[24] = yspc2;

  ys2 = yscol + yspc2;
  output_[25] = ys2;

  hec_calc_ = 0;
  if ( case_hec == 1 ) {
    hec_calc_ = ys1;
  } else if ( case_hec == 2 ) {
    hec_calc_ = ys2;
  } else if ( case_hec == 12 ) {
    hec_calc_ = Math.max(ys1, ys2);
  } 

  output_[26] = hec_calc_;

  return output_;
}

function fminmax(x, xmin, xmax){
  return Math.min(Math.max(x, xmin), xmax);
}

function tanh(x){
  temp=0;
  if(x>12){
    temp=1;
  } else if (x<-12) {
    temp=-1;
  } else {
  return (Math.exp(x)-Math.exp(-1*x))/(Math.exp(x)+Math.exp(-1*x));
  }
}


function fdotSingle(y, V, d50, b){
  var result_=[5] ;
  var Vc, ustarc, z0, thetac, sg, g, dstar, nu, Rec, ks;
  var Vlp=0 ;
  var ys=0 ;
  var bd50 ;
  var f1 ;
  var f3 ;
  var i ;
  var sg = 2.65;
  var g = 9.81;
  var nu = 0.000001004;
  var Vc = 0 , ustarc = 0 , z0 = 0 , thetac = 0 , dstar = 0 , Rec = 0 , ks = 0;  
  
  for (i=0;i<=4;i++){
    result_[i] = 0;
  }
  
  var temp = result_;
  
  if( y > 0 && V > 0 && d50 > 0 && b > 0 ){
    
    dstar = d50 / 1000 * Math.pow((sg - 1) * g / Math.pow(nu,2),1 / 3);
    
    if( d50 >= 0.6 ){ ks = 2.5 * d50 / 1000;}
    if( d50 < 0.6 ){ ks = 5 * d50 / 1000;}
    
    if( 0.01 < dstar && dstar <= 3 ){ thetac = 0.25 - 0.1 * Math.sqrt(dstar);}
    if( 3 < dstar && dstar <= 150 ){ thetac = 0.0023 * dstar - 0.000378 * dstar * Math.log(dstar) + 0.23 / dstar - 0.005;}
    if( dstar > 150 ){ thetac = 0.0575;}
    
    ustarc = Math.sqrt(thetac * (sg - 1) * g * d50 / 1000);
    
    Rec = ustarc * ks / nu;
    
    if( 0 < Rec && Rec <= 5 ){ z0 = nu / (9 * ustarc);}
    if( 5 < Rec && Rec <= 70 ){ z0 = ks * Math.pow(10,-3) * (-6 + 2.85 * Rec - 0.58 * Rec * Math.log(Rec) + 0.002 * Math.pow(Rec,2)+ 111 / Rec);}
    if( Rec > 70 ){ z0 = ks / 30;}
    
    Vc = 2.5 * ustarc * Math.log(y / (2.72 * z0));
    
    
    Vlp = Math.max(5 * Vc, 0.6 * Math.sqrt(g * y));
    
    
    
    bd50 = b / (d50 / 1000);
    
    
    f1 = tanh(Math.pow(y / b,0.4))
    f3 = bd50 / (0.4 * Math.pow(bd50,1.2) + 10.6 * Math.pow(bd50,-0.13));
    
    if( V / Vc < 0.4 ){
      ys = 0;
    }else if( 0.4 <= V / Vc && V / Vc <= 1 ){
      ys = b * 2.5 * f1 * (1 - 1.2 * Math.pow(Math.log(V / Vc),2)) * f3;
    }else if( 1.0 < V / Vc && V / Vc <= Vlp / Vc ){
      ys = b * f1 * (2.2 * (V / Vc - 1) / (Vlp / Vc - 1) + 2.5 * f3 * (Vlp / Vc - V / Vc) / (Vlp / Vc - 1));
    }else if( V / Vc > Vlp / Vc ){
      ys = b * 2.2 * f1;
    }

  }
  
  result_[0] = Vc;
  result_[1] = Vlp;
  result_[2] = ys;
  return result_;
}

function readerA(d50, y, V, sigma_g, bcol, Kscol, bpc, T, f1, f2, h0, h1, Kspc, bp, m, n, Sn, Sm, Kspg) {
  
  //  temp=0;
  //  temp=d50+y+V+sigma_g+bcol+Kscol+bpc+T+f1+f2+h0+h1+Kspc+bp+m+n+Sn+Sm+Kspg;
  //  return temp;
  
  var type_="complex pier";
  var res_=0;
  //var d50 = 0 ,  y = 0 ,  V = 0 ,  sigma_g = 0 ,  bcol = 0 ,  Kscol = 0 ,  bpc = 0 ,  T = 0 ,  f1 = 0 ,  f2 = 0 ,  h0 = 0 ,  h1 = 0 ,  Kspc = 0 ,  bp = 0 ,  m = 0 ,  n = 0 ,  Sn = 0 ,  Sm = 0 ,  Kspg =0;
  var result_ =[26];
  var row_now=0;
  var err_ =[20];
  var ii = 0;
  var z =0;
  var ysHEC = 0 ,  bstar3_ = 0 ,  bpg=0;
  var be_NN_case_1111 = 0 ,  be_NN_case_3343 = 0 ,  be_NN_all = 0 ,  be_SVM = 0 ,  be_GP = 0 ,  be_ANFIS = 0 ,  be_M5_AminiBagh =0;;
  var ys_NN_case_1111 = 0 ,  ys_NN_case_3343 = 0 ,  ys_NN_all = 0 ,  ys_SVM = 0 ,  ys_GP = 0 ,  ys_ANFIS = 0 ,  ys_M5_AminiBagh =0;
  var x1=0, x2=0, x3=0, x4=0, x5=0, x6=0, x7=0, case_=0;
  var jj=0;
  var Vc=0, Vlp=0, ys=0;
  var result__ =[5];
  var row_now = 0;
  var x1=0, x2=0, x3=0, x4=0, x5=0, x6=0, x7=0, case_=0;
  var jj=0;
  var Vc=0, Vlp=0, ys=0;
  var result__ =[5];
  var row_now = 0;
  var temp=0;
  
  case_=caseFinder(type_,h0,h1,y,bcol);
  
  resultHec=hecCalc(type_, d50 , sigma_g , y , V , bcol , Kscol , bpc , T , f1 , h0 , h1 , Kspc , bp , m , n , Sn , Sm , Kspg ) ;
  
  var stars_=[4];
  stars_[0]="*";
  stars_[1]="*";
  stars_[2]="*";
  stars_[3]="*";
  stars_[4]="*";
  
  resultt1=resultHec.concat(stars_);
  var newResult=[13];
  z = resultt1[26];
  ysHEC = resultt1[26];
  
  bpg = bpgHEC(bp, n, m, Sn, Sm);
  newResult[0]=bpg;
  
  bstar3_ = bstar3(h0, h1, y, ysHEC, bcol, bpc, bpg, Kscol, Kspc, Kspg);
  newResult[1]=bstar3_;
  
  if(case_ == 2 || case_ == 3 || case_ == 4 || case_ == 5 ){
    x1 = bpg / bpc; 
  }
  
  if(case_ == 3 || case_ == 4 || case_ == 5 || case_ == 7 || case_ == 8 ){ 
    x2 = bcol / bpc; 
  }
  
  if(case_ == 2 || case_ == 3 || case_ == 4 || case_ == 5 ){ 
    x3 = Math.max(-2.4 * bcol, h0) / y; 
  }
  
  if(case_ == 2 ){ x4 = 1;
  }else if( case_ == 3 || case_ == 4 || case_ == 5 || case_ == 7 || case_ == 8 ){ 
    x4 = h1 / y; 
  }
  
  if(case_ == 2 ){
    x5 = 1 - h0 / y;
  }else if( case_ == 3 ){
    x5 = T / y;
  }else if( case_ == 4 || case_ == 5 ){ 
    x5 = Math.min(T, h1 + 2.4 * bcol) / y;
  }
  
  if(case_ == 2 || case_ == 3 || case_ == 4 || case_ == 5 || case_ == 7 || case_ == 8 ){ 
    x6 = bpc / y;
  }
  
  if(case_ == 3 || case_ == 4 || case_ == 5 || case_ == 7 || case_ == 8 ){
    x7 = f1 / bcol; 
  }
  
  //newResult[2]=x1+x2+x3+x4+x5+x6+x7;
  
  be_NN_case_1111 = bstar3_ * nnCase(x1, x2, x3, x4, x5, x6, x7, case_);
  be_NN_case_3343 = bstar3_ * nnCase(x1, x2, x3, x4, x5, x6, x7, case_);
  be_GP = bstar3_ * gpCase(x1, x2, x3, x4, x5, x6, x7, case_);
  be_ANFIS = bstar3_ * anfisAll(x1, x2, x3, x4, x5, x6, x7, case_);
  be_M5_AminiBagh = bstar3_ * M5AminiBagh18Update(y, bcol, Kscol, bpc, T, f1, h0, h1, Kspc, bpg, Kspg, case_);
                  be_SVM = bstar3_ * SVMforAll(x1, x2, x3, x4, x5, x6, x7, 107, case_);
  //be_GP=1;
  
  newResult[2]=be_NN_case_1111;
  
  temp = fdotSingle(y, V, d50, be_NN_case_1111);
  Vc = temp[0];
  Vlp = temp[1];
  ys_NN_case_1111 = temp[2];
  
  temp = fdotSingle(y, V, d50, be_NN_case_3343);
  ys_NN_case_3343 = temp[2];  
  
  temp = fdotSingle(y, V, d50, be_GP);
  ys_GP = temp[2];
  
  temp = fdotSingle(y, V, d50, be_ANFIS);
  ys_ANFIS = temp[2];
  
  temp = fdotSingle(y, V, d50, be_M5_AminiBagh);
  ys_M5_AminiBagh = temp[2];
  
  temp = fdotSingle(y, V, d50, be_SVM);
  ys_SVM= temp[2];
  
  newResult[3]=Vc;
  newResult[4]=Vlp;
  newResult[5]=ys_NN_case_1111;
  newResult[6]=ys_NN_case_3343;
  newResult[7]=0; //NN ALL 
  newResult[8]=ys_SVM; // SVM
  newResult[9]=ys_GP; //GP
  newResult[10]=ys_ANFIS; 
  newResult[11]=ys_M5_AminiBagh;
  
  
  
  
  resultt2=resultt1.concat(newResult);

  return [resultt2];
  return bpg;
}
