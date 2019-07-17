function M5AminiBagh18Update(y, bcol, Kscol, bpc, T, f1, h0, h1, Kspc, bpg, Kspg, case_){
  
  var z;
  
  if(case_ == 2 ){
    
    z = 0.524 * Math.pow(h0 / y + 0.1,-0.226) * Math.pow(bpc / bpg,0.184);
    
  }else if( case_ == 3 ){
    
    if(bpg / bpc <= 0.326 ){
      z = 0.384 * Math.pow(bpg / bpc + 0.1,-0.242) * Math.pow(T / y + 0.1,-0.169) * Math.pow(bcol / bpc,-0.213) * Math.pow(h1 / y + 0.1,0.05);
    }else{
      z = 1.125 * Math.pow(bpg / bpc + 0.1,0.746) * Math.pow(T / y + 0.1,-0.009) * Math.pow(bcol / bpc,0.078) * Math.pow(h1 / y + 0.1,-0.273);
    }
    
  }else if( case_ == 4 ){
    
    if(bcol / bpc <= 0.42 ){
      z = 1.512 * Math.pow(T / y + 0.1,-0.428) * Math.pow(bcol / bpc,0.965) * Math.pow(f1 / bcol + 0.1,-0.271) * Math.pow(bpg / bpc + 0.1,-0.013);
    }else{
      z = 1.81 * Math.pow(T / y + 0.1,-0.287) * Math.pow(bcol / bpc,0.825) * Math.pow(f1 / bcol + 0.1,0.191) * Math.pow(bpg / bpc + 0.1,0.617)
    }
    
  }else if( case_ == 5 ){
    
    if(f1 / bcol <= 0.4 ){
      z = 0.97 * Math.pow(bpc / bcol,-0.116) * Math.pow(0.1 - h1 / y,0.085) * Math.pow(f1 / bcol + 0.1,-0.03);
    }else if( f1 / bcol <= 0.75 ){
      z = 0.912 * Math.pow(bpc / bcol,-0.031) * Math.pow(0.1 - h1 / y,0.394) * Math.pow(f1 / bcol + 0.1,0.814);
    }else{
      z = 1.645 * Math.pow(bpc / bcol,-0.469) * Math.pow(0.1 - h1 / y,0.266) * Math.pow(f1 / bcol + 0.1,-0.711);
    }
    
  }else if( case_ == 1 || case_ == 6 ){
    z = 1;
  }
  
  
  return z;
  
}