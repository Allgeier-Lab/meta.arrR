#include "rcpp_list_to_matrix.h"

//' rcpp_list_to_matrix
//'
//' @description Rcpp list to matrix
//'
//' @param x List with individuals within metaecosystems.
//' @param n Integer with number of metaecosystems.
//' @param pop_n Integer with number of indiviuals.
//'
//' @details
//' Converts list with individuals within metaecosystems to one matrix. Adds coloumn
//' with metasystem id
//'
//' @return matrix
//'
//' @aliases rcpp_list_to_matrix
//' @rdname rcpp_list_to_matrix
//'
//' @keywords export
// [[Rcpp::export]]
Rcpp::NumericMatrix rcpp_list_to_matrix(Rcpp::List x, int n, int pop_n) {

  // creating final matrix with local systems times pop_n rows and additional col for id
  Rcpp::NumericMatrix result(n * pop_n, 15);

  // counter to fill rows
  int k = 0;

  // loop through all list elements
  for (int i = 0; i < x.length(); i ++) {

    // get current temp element
    Rcpp::NumericMatrix input_temp = x[i];

    // loop through all rowis of temp element
    for (int j = 0; j < input_temp.nrow(); j++) {

      // check if row is empty
      bool id_temp = all(Rcpp::is_na(input_temp(j, _)));

      if (id_temp) continue;

      // get current row
      Rcpp::NumericVector row_temp = input_temp(j, _);

      // add meta id
      row_temp.insert(14, i + 1);

      // write into current row
      result(k, _) = row_temp;

      // increase counter
      k += 1;

    }
  }

  return result;
}

/*** R
fishpop_values <- lapply(metasyst$fishpop, function(i)
  as.matrix(raster::as.data.frame(i, xy = TRUE)))

rcpp_list_to_matrix(x = fishpop_values, n = metasyst$n,
                    pop_n = metasyst$starting_values$pop_n)

rcpp_list_to_matrix(x = lst, n = metasyst$n,
                    pop_n = metasyst$starting_values$pop_n)
*/
