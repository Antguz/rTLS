// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// cartesian_to_polar_rcpp
NumericMatrix cartesian_to_polar_rcpp(NumericMatrix cartesian, NumericVector anchor);
RcppExport SEXP _rTLS_cartesian_to_polar_rcpp(SEXP cartesianSEXP, SEXP anchorSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type cartesian(cartesianSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type anchor(anchorSEXP);
    rcpp_result_gen = Rcpp::wrap(cartesian_to_polar_rcpp(cartesian, anchor));
    return rcpp_result_gen;
END_RCPP
}
// dimensionality_knn_rcpp
arma::cube dimensionality_knn_rcpp(arma::mat amat, arma::mat bmat, arma::vec k, int threads, bool progress);
RcppExport SEXP _rTLS_dimensionality_knn_rcpp(SEXP amatSEXP, SEXP bmatSEXP, SEXP kSEXP, SEXP threadsSEXP, SEXP progressSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type amat(amatSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type bmat(bmatSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type k(kSEXP);
    Rcpp::traits::input_parameter< int >::type threads(threadsSEXP);
    Rcpp::traits::input_parameter< bool >::type progress(progressSEXP);
    rcpp_result_gen = Rcpp::wrap(dimensionality_knn_rcpp(amat, bmat, k, threads, progress));
    return rcpp_result_gen;
END_RCPP
}
// dimensionality_sphere_rcpp
arma::cube dimensionality_sphere_rcpp(arma::mat amat, arma::mat bmat, arma::vec radius, int threads, bool progress);
RcppExport SEXP _rTLS_dimensionality_sphere_rcpp(SEXP amatSEXP, SEXP bmatSEXP, SEXP radiusSEXP, SEXP threadsSEXP, SEXP progressSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type amat(amatSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type bmat(bmatSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type radius(radiusSEXP);
    Rcpp::traits::input_parameter< int >::type threads(threadsSEXP);
    Rcpp::traits::input_parameter< bool >::type progress(progressSEXP);
    rcpp_result_gen = Rcpp::wrap(dimensionality_sphere_rcpp(amat, bmat, radius, threads, progress));
    return rcpp_result_gen;
END_RCPP
}
// distanceC
NumericVector distanceC(double xcoor, double ycoor, double zcoor, NumericVector X, NumericVector Y, NumericVector Z);
RcppExport SEXP _rTLS_distanceC(SEXP xcoorSEXP, SEXP ycoorSEXP, SEXP zcoorSEXP, SEXP XSEXP, SEXP YSEXP, SEXP ZSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type xcoor(xcoorSEXP);
    Rcpp::traits::input_parameter< double >::type ycoor(ycoorSEXP);
    Rcpp::traits::input_parameter< double >::type zcoor(zcoorSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type X(XSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type Y(YSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type Z(ZSEXP);
    rcpp_result_gen = Rcpp::wrap(distanceC(xcoor, ycoor, zcoor, X, Y, Z));
    return rcpp_result_gen;
END_RCPP
}
// euclidean_distance
Rcpp::NumericVector euclidean_distance(Rcpp::NumericVector sample, Rcpp::NumericMatrix base);
RcppExport SEXP _rTLS_euclidean_distance(SEXP sampleSEXP, SEXP baseSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type sample(sampleSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type base(baseSEXP);
    rcpp_result_gen = Rcpp::wrap(euclidean_distance(sample, base));
    return rcpp_result_gen;
END_RCPP
}
// minimun_distance_rcpp
double minimun_distance_rcpp(arma::mat amat, int threads, bool progress);
RcppExport SEXP _rTLS_minimun_distance_rcpp(SEXP amatSEXP, SEXP threadsSEXP, SEXP progressSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type amat(amatSEXP);
    Rcpp::traits::input_parameter< int >::type threads(threadsSEXP);
    Rcpp::traits::input_parameter< bool >::type progress(progressSEXP);
    rcpp_result_gen = Rcpp::wrap(minimun_distance_rcpp(amat, threads, progress));
    return rcpp_result_gen;
END_RCPP
}
// polar_to_cartesian_rcpp
NumericMatrix polar_to_cartesian_rcpp(NumericMatrix polar, int threads);
RcppExport SEXP _rTLS_polar_to_cartesian_rcpp(SEXP polarSEXP, SEXP threadsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type polar(polarSEXP);
    Rcpp::traits::input_parameter< int >::type threads(threadsSEXP);
    rcpp_result_gen = Rcpp::wrap(polar_to_cartesian_rcpp(polar, threads));
    return rcpp_result_gen;
END_RCPP
}
// rotate_rcpp
NumericMatrix rotate_rcpp(NumericMatrix cloud, NumericVector roll, NumericVector pitch, NumericVector yaw);
RcppExport SEXP _rTLS_rotate_rcpp(SEXP cloudSEXP, SEXP rollSEXP, SEXP pitchSEXP, SEXP yawSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type cloud(cloudSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type roll(rollSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type pitch(pitchSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type yaw(yawSEXP);
    rcpp_result_gen = Rcpp::wrap(rotate_rcpp(cloud, roll, pitch, yaw));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_rTLS_cartesian_to_polar_rcpp", (DL_FUNC) &_rTLS_cartesian_to_polar_rcpp, 2},
    {"_rTLS_dimensionality_knn_rcpp", (DL_FUNC) &_rTLS_dimensionality_knn_rcpp, 5},
    {"_rTLS_dimensionality_sphere_rcpp", (DL_FUNC) &_rTLS_dimensionality_sphere_rcpp, 5},
    {"_rTLS_distanceC", (DL_FUNC) &_rTLS_distanceC, 6},
    {"_rTLS_euclidean_distance", (DL_FUNC) &_rTLS_euclidean_distance, 2},
    {"_rTLS_minimun_distance_rcpp", (DL_FUNC) &_rTLS_minimun_distance_rcpp, 3},
    {"_rTLS_polar_to_cartesian_rcpp", (DL_FUNC) &_rTLS_polar_to_cartesian_rcpp, 2},
    {"_rTLS_rotate_rcpp", (DL_FUNC) &_rTLS_rotate_rcpp, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_rTLS(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
