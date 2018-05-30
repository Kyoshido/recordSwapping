#include <Rcpp.h>  // needed for using this in R
#include <iostream>     
#include <algorithm>    // std::count
#include <vector>       // std::vector
#include <random>
#include <queue>
#include <ctime>        // time()

// Enable C++11 via this plugin (Rcpp 0.10.3 or later) - used for R only
// [[Rcpp::plugins(cpp11)]] 

using namespace std;
using namespace Rcpp; // needed for using this in R

/*
 * Function to reorder data-set given one index vector 
 */
// [[Rcpp::export]]
std::vector< std::vector<int> > orderData(std::vector< std::vector<int> > data, int orderIndex){
  
  // initialise ordering vector
  std::vector<int> orderVec(data[0].size());
  std::iota(orderVec.begin(),orderVec.end(),0);
  
  // order this vector by order of data[orderIndex]
  std::sort(orderVec.begin(),orderVec.end(),
    [&](int a, int b) { return data[orderIndex][a] < data[orderIndex][b]; }
  );

  // reorder data without copying it
  for(int i=0;i<orderVec.size();i++){
    // while orderVec[i] is not yet in place 
    // every swap places at least one element in it's proper place
    while(orderVec[i] !=   orderVec[orderVec[i]] ){
      // swap every "column" of data
      for(int j=0;j<data.size();j++){
        swap( data[j][orderVec[i]], data[j][orderVec[orderVec[i]]] );
      }
      // then adjust orderVec[i]
      swap( orderVec[i], orderVec[orderVec[i]] );
    }
  }
  
  return(data);
}


/*
* Function to define levels 
*/
// [[Rcpp::export]]
std::vector<int> setLevels(std::vector< std::vector<int> > data, std::vector<int> hierarchy, std::vector<int> risk, int hid, int th) {
  
  // data: data input
  // hierarchy: column indices in data corresponding to geo hierarchy of data read left to right (left highest level - right lowest level)
  // risk: column indices in data corresponding to risk variables which will be considered for estimating counts in the population
  // hid: int correspondig to column index in data which holds the household ID
  // th: int defining a threshold, each group with counts lower than the threshold will automatically be swapped.
  
  
  // initialise parameters
  int p = data.size();  // number of columns in data
  int n = data[0].size();  // number of rows in data
  
  int nhier = hierarchy.size();
  
  // initialise map
  std::map<std::vector<int>,int> group_count; 

  std::vector<int> loop_index=risk;
  loop_index.insert(loop_index.end(),hierarchy.begin(),hierarchy.end());
  int loop_n = loop_index.size();
  // initialise vector for groups
  std::vector<int> groups(loop_n);
  // initialise map for household size
  std::map<int,int> map_hsize;
  
  ////////////////////////////////////////////////////
  // loop over data and ...
  for(int i=0;i<n;i++){
    
    // ... define group
    for(int j=0;j<loop_n;j++){
      groups[j] = data[loop_index[j]][i];
    }

    // ... count number for each group using std::map
    group_count[groups]++;
    
    // ... get household size map
    map_hsize[data[hid][i]]++;
  }
  ////////////////////////////////////////////////////

  ////////////////////////////////////////////////////
  // iterate over level_count and 
  // set level number for group if Number of obs in group <= th
  ////////////////////////////////////////////////////
  // initialise map with levels for each group
  std::map<std::vector<int>,int> level_number = group_count; 

  for(int i = 0;i<nhier;i++){
    // int i=2;  
    std::map<std::vector<int>,int> group_count_help = group_count;
    if(i>0){
      std::map<std::vector<int>,int> group_higher_help;
      
      // count occurences for higher grouping
      for(auto const&x : group_count_help){
        std::vector<int> groups_help = x.first;
        groups_help.resize(loop_n-i);
        group_higher_help[groups_help] += x.second; 
      }
      // loop over group_count_help again
      // and set the values to the map values in group_higher_help
      // those are the aggregates values
      for(auto const&x : group_count_help){
        std::vector<int> groups_help = x.first;
        groups_help.resize(loop_n-i);
        group_count_help[x.first] = group_higher_help[groups_help];
      }
    }
    
    for(auto const& x : group_count_help){ // -> loop over map entries
      if(x.second <= th){ // -> x.second = number of obs
        level_number[x.first] = nhier-i-1;
      }else{
        if(i==0){
          level_number[x.first] = nhier; //initialise with lowest level only at first loop
        }
      }
    }
  }
  ////////////////////////////////////////////////////
  
  //////////////////////////////////////////////////// 
  // initialise return vector for levels
  std::vector<int> data_level(n);
  int hsize=0;
  
  int i =0;
  int min_level=nhier;
  while(i<n){
    
    // get hsize of record i
    hsize = map_hsize[data[hid][i]];
    for(int h=0;h<hsize;h++){
      // ... define group
      for(int j=0;j<loop_n;j++){
        groups[j] = data[loop_index[j]][i+h];
      }
      min_level = std::min(min_level,level_number[groups]);
    }
    // apply minimum level to all data_level per hid
    for(int h=0;h<hsize;h++){
      data_level[i+h] = min_level;
    }
    i = i+hsize;
    min_level=nhier; // set min_level to nhier for each hid
  }

  ////////////////////////////////////////////////////
  return data_level;
}


/*
 * Function to set sampling probability 
 * and reverse sampling probability (for donor sets)
 */
// [[Rcpp::export]]
std::vector< std::vector<double> > setRisk(std::vector<std::vector<int> > data, std::vector<int> hierarchy, std::vector<int> risk, int hid){
  
  // data: data input
  // hierarchy: column indices in data corresponding to geo hierarchy of data read left to right (left highest level - right lowest level)
  // risk: column indices in data corresponding to risk variables which will be considered for estimating counts in the population
  // hid: int correspondig to column index in data which holds the household ID

  // initialise parameters
  int n = data[0].size();
  int nhier = hierarchy.size();
  double risk_value=0;
  int current_ID;
  int hsize=0;
  // initialise probability data
  // index 0 corresponds to sampling probability for swapping
  // index 1 corresponds to sampling probability for donor set
  std::vector< std::vector<double> > prob(2, vector<double>(n));
  
  //
  std::vector<int> loop_index = risk;
  loop_index.insert(loop_index.end(),hierarchy.begin(),hierarchy.end());
  int loop_n = loop_index.size();
  std::vector<int> groups(loop_n);
  
  // initialise counts for groups in lowest hierarchy
  std::map<std::vector<int>,int> group_count; 
  for(int i=0;i<n;i++){
    
    // ... define group
    for(int j=0;j<loop_n;j++){
      groups[j] = data[loop_index[j]][i];
    }
    
    // ... count number for each group using std::map
    group_count[groups]++;
  }
  
  // loop again over data
  int i=0;
  int h=0;
  while(i<n){
    
    current_ID = data[hid][i];
    while(current_ID==data[hid][i+h]){
      // ... define group
      for(int l=0;l<loop_n;l++){
        groups[l] = data[loop_index[l]][i+h];
      }

      // select highest risk for hid
      risk_value = max(risk_value,1.0/group_count[groups]);
      hsize++;
      h++;
    }
    // assign highst risk and revers risk to all household members
    for(int j=0;j<hsize;j++){
      prob[0][i+j] = risk_value;
      if(risk_value<1){
        prob[1][i+j] = 1.0 - risk_value;
      }else{
        prob[1][i+j] = 5e-10;
      }
    }
    
    // reset parameters
    i = i+hsize;
    hsize=0;
    risk_value = 0;
    h =0;
  }
  
  return prob;
}



/*
 * Function to sample from std::vector<int> given a probability vector
 */
// [[Rcpp::export]]
std::vector<int> randSample(std::vector<int> ID, int N, std::vector<double> prob,int seed){
 
 // initialise parameters
 int n = ID.size();
 std::vector<int> sampleID(N);
 std::vector<double> randVal(n);
 /*
  * from R-Package ‘wrswoR’:
  * We need the last "size" elements of
  * U ^ (1 / prob) ~ log(U) / prob
  * ~ -Exp(1) / prob
  * ~ prob / Exp(1)
  * Here, ~ means "doesn't change order statistics".
  */  
 
 std::mt19937_64 mersenne_engine; // move this outside the function when finished testing!
 mersenne_engine.seed(seed); // set random seed according to time
 std::exponential_distribution<double> exp_dist(1.0); // initialise lambda para for exp distribution
 
 // fill vector with prob/(random number)
 for(int i=0;i<n;i++){
   randVal[i] = prob[i]/exp_dist(mersenne_engine);
 }
 // return randVal;
 // get index of N largest elements in randVal
 // from https://stackoverflow.com/questions/14902876/indices-of-the-k-largest-elements-in-an-unsorted-length-n-array
 // use priority_queue
 
 std::priority_queue<std::pair<double, int>> q;
 for (int i = 0; i < n; ++i) {
   q.push(std::pair<double, int>(randVal[i], i));
 }
 // select index of top elements from priority_queue
 for(int i=0;i<N;i++){
   sampleID[i] = ID[q.top().second]; //.top() access top element in queue
   q.pop(); // remove top element in queue
 }

 return sampleID;
}


/*
 * Function to define number of draws at each geo hierarchy
 */
// [[Rcpp::export]]
std::vector<int> setNdraw(std::vector< std::vector<int> > data, std::vector<int> hierarchy, double swap){
  
  // data: data input
  // hierarchy: column indices in data corresponding to geo hierarchy of data read left to right (left highest level - right lowest level)
  // swap: double defining the ratio of households to be swapped
  
  // initialise parameter
  int n = data[0].size();
  int nhier = hierarchy.size();
  
  
} 


/*
 * Function to perform record swapping
 */
std::vector<vector<int>> recordSwap(std::vector< std::vector<int> > data, std::vector<int> similar,
                                    std::vector<int> hierarchy, std::vector<int> risk, int hid, int th, double swap){
  
  // data: data input
  // hierarchy: column indices in data corresponding to geo hierarchy of data read left to right (left highest level - right lowest level)
  // similar: column indices in data corresponding to variables (household/personal) which should be considered when swapping,
  // e.g. swapping onlys household with same houshoeld size 
  // risk: column indices in data corresponding to risk variables which will be considered for estimating counts in the population
  // hid: int correspondig to column index in data which holds the household ID
  // th: int defining a threshold, each group with counts lower than the threshold will automatically be swapped.
  // swap: double defining the ratio of households to be swapped
  
  
  // initialise parameter
  int n = data[0].size();
  std::vector<int> levels(n);
  std::vector< std::vector<double> > prob(2, vector<double>(n));
  
  ////////////////////////////////////////////////////
  // order data by hid 
  data = orderData(data,hid);
  ////////////////////////////////////////////////////
  
  ////////////////////////////////////////////////////
  // define minimum swap level for each household
  levels = setLevels(data,hierarchy,risk,hid,th);
  ////////////////////////////////////////////////////
    
  ////////////////////////////////////////////////////
  // define sampling probabilities
  prob = setRisk(data, hierarchy, risk, hid);
  ////////////////////////////////////////////////////
  
  ////////////////////////////////////////////////////
  // define number of swaps on each level (discard this step????)
  
  
  ////////////////////////////////////////////////////
  // apply swapping algorithm
  // go from highest to lowest level
  // swapp at each higher level the number of households that have to be swapped at that level according to "th" (see setLevels())
  // at lowest level swap remaining number of households (according to swap) if not enough households have been swapped
  // every household can only be swapped once 
  
  
  
  
}






