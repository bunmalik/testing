public static void MCSD(VrpSolution sol, int n, VrpProblem problem) throws ScriptException{
        // https://support.microsoft.com/en-us/office/introduction-to-monte-carlo-simulation-in-excel-64c0ba99-752a-4fa8-bbd3-4450d8db16f1
        // Note 
        // fitdsitrubution to the demand data
        // generate probabilites for the data
        // using the mean and variance, generate an inverse lognormal of the demand data
        // use monte carlo to simulate and generate estimated data
        // run the         

        // get the demand data
        // List<Double> demand = new ArrayList();
        for (int i=0;i<=problem.getNumCities()-1;i++){
            demand.add(problem.getDemands()[i]);
        }
        
        // fit data to distribution
        RenjinScriptEngine engine = new RenjinScriptEngine();
        engine.put("x", new DoubleArrayVector(demand));
        engine.eval("data<-as.data.frame(list(x))");
        engine.eval("library(fitdistrplus)");
        engine.eval("fit <- fitdist(data[,1], \"lnorm\")");
//        engine.eval("print(summary(fit))");
        //  double mean = (double) engine.eval("fit$meanlog");
        //  double sdlog = (double) engine.eval("fit$meanlog");
        
       // run the monte carlo
       List<Double> estimatedemands = new ArrayList();
       List<Double> icdf = new ArrayList();
       
        for (int d =0;d < demand.size();d++){
           List<Double> randnumbers = randomnumbers(1);     
           engine.put("p", new DoubleArrayVector(randnumbers));
//           engine.eval("p <- plnorm(data[,1], meanlog = fit$estimate[\"meanlog\"], sdlog = fit$estimate[\"meanlog\"])");
           for(int i=0; i < n; i++){
               engine.put("p", new DoubleArrayVector(randnumbers));
               DoubleVector res = (DoubleVector)engine.eval("q <- qlnorm(p, meanlog = fit$estimate[\"meanlog\"], sdlog = fit$estimate[\"sdlog\"])");
               icdf.add(res.get(0));
           }
           double means = mean(icdf);
           icdf = new ArrayList();
           estimatedemands.add(means);
        }
  }
  
  public static void MCSTT(VrpProblem problem, int n) throws FileNotFoundException, ScriptException{
        RenjinScriptEngine engine = new RenjinScriptEngine();
        engine.eval(new java.io.FileReader("C:\\Users\\bunmalik\\Documents\\NetBeansProjects\\MonteCarlo\\montecarlo_traveltimes.R"));
        engine.eval("res<-dat()");
        Object data = engine.eval("data<-res$data");
        IntArrayVector m = (IntArrayVector) engine.eval("m<-length(colnames(data))");
        DoubleArrayVector cols = (DoubleArrayVector) engine.eval("cols<-res$cols");
        DoubleArrayVector rows = (DoubleArrayVector) engine.eval("cols<-res$rows");
        List<Double> distanceFromDepot1 = new ArrayList();
        List<List<Double>> cityDists1 = new ArrayList();
        List<Double> cityDist1 =   new ArrayList();
        
        for( int l=1; l <= m.asInt(); l++){
        engine.put("l", (int) l);
        engine.eval("fit <- fitting(data,l)");
        cityDist1.add(MCSTT(n,m,engine,problem));
    }
        double[][] cityDists = toArray(cityDist1,rows.asInt());
  }
  
  public static double[][] toArray(List<Double> list, int rows) {
    double[][] result = new double[rows][rows];
    double[] distFromDepot = new double[rows];
    double[] distToDepot = new double[rows];
    
    // insert 0.0 at diagonal positions
    for(int i= 0; i<list.size(); i++){
        if(i%11==0)
            list.add(i, 0.0);
    }
    list.add(0.0);
    
    // convert list to 2d array
    int counter = 0;
    for (Double value : list) {
        int row = counter/rows;
        int col = counter%rows;   
        result[row][col] = value;
        counter++;
    }
    
    // save cost of nodes from depot 
    for (int col = 0; col < result[0].length; col++) {       
       distFromDepot[col] = result[0][col];
    }
    
    // remove first row and first column after getting costs from depot
    double cityDists[][] = new double[rows-1][rows-1];
    int REMOVE_ROW = 0;
    int REMOVE_COLUMN = 0;
    int p = 0;
    for( int i = 0; i < rows; ++i)
        {
            if ( i == REMOVE_ROW)
                continue;
            int q = 0;
            for( int j = 0; j < rows; ++j)
            {
                if ( j == REMOVE_COLUMN)
                    continue;
                cityDists[p][q] = result[i][j];
                ++q;
            }
            ++p;
        }
    return cityDists;
}
  public static double MCSTT(int n, IntArrayVector m, RenjinScriptEngine engine,VrpProblem problem) throws ScriptException, FileNotFoundException{
        List<Double> estimatetraveltimes = new ArrayList();
        List<Double> icdf = new ArrayList();
        double means=0;
        
        for(int i=1; i <= n; i++){
               List<Double> randnumbers = randomnumbers(1); 
               engine.put("p", new DoubleArrayVector(randnumbers));
               DoubleVector ress = (DoubleVector)engine.eval("sample <- qshiftlnorm(p,  mean = fit$estimate['mean'], sigma=fit$estimate['sigma'], shift = fit$estimate['shift'])");
               icdf.add(ress.get(0));
           }
            means = mean(icdf);
            icdf = new ArrayList();
            estimatetraveltimes.add(means);
        return means;
  }
  
  
  
  public static List<Double> randomnumbers(int n){
      Random rand = new Random();
                List<Double> randnumbers = new ArrayList();
                for (int i=0;i<n;i++){
                    double numbers = rand.nextDouble(1);
                    randnumbers.add(numbers);
                }
        return randnumbers;
  }
  
  public static double mean(List<Double> list){
      double sum = 0;
      for (double d: list ){
          sum +=d;
      }
     return sum/list.size();
  }
  