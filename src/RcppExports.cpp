// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// cartesian_to_polar_rcpp
NumericMatrix cartesian_to_polar_rcpp(NumericMatrix cartesian, NumericVector anchor, int threads);
RcppExport SEXP _rTLS_cartesian_to_polar_rcpp(SEXP cartesianSEXP, SEXP anchorSEXP, SEXP threadsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type cartesian(cartesianSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type anchor(anchorSEXP);
    Rcpp::traits::input_parameter< int >::type threads(threadsSEXP);
    rcpp_result_gen = Rcpp::wrap(cartesian_to_polar_rcpp(cartesian, anchor, threads));
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
// euclidean_distance
Rcpp::NumericVector euclidean_distance(Rcpp::NumericVector sample, Rcpp::NumericMatrix base, int threads);
RcppExport SEXP _rTLS_euclidean_distance(SEXP sampleSEXP, SEXP baseSEXP, SEXP threadsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type sample(sampleSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type base(baseSEXP);
    Rcpp::traits::input_parameter< int >::type threads(threadsSEXP);
    rcpp_result_gen = Rcpp::wrap(euclidean_distance(sample, base, threads));
    return rcpp_result_gen;
END_RCPP
}
// meanDis_knn_rcpp
arma::vec meanDis_knn_rcpp(arma::mat amat, int k, int threads, bool progress);
RcppExport SEXP _rTLS_meanDis_knn_rcpp(SEXP amatSEXP, SEXP kSEXP, SEXP threadsSEXP, SEXP progressSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type amat(amatSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< int >::type threads(threadsSEXP);
    Rcpp::traits::input_parameter< bool >::type progress(progressSEXP);
    rcpp_result_gen = Rcpp::wrap(meanDis_knn_rcpp(amat, k, threads, progress));
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
// nneighbors_sphere_rcpp
arma::vec nneighbors_sphere_rcpp(arma::mat amat, double radius, int threads, bool progress);
RcppExport SEXP _rTLS_nneighbors_sphere_rcpp(SEXP amatSEXP, SEXP radiusSEXP, SEXP threadsSEXP, SEXP progressSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type amat(amatSEXP);
    Rcpp::traits::input_parameter< double >::type radius(radiusSEXP);
    Rcpp::traits::input_parameter< int >::type threads(threadsSEXP);
    Rcpp::traits::input_parameter< bool >::type progress(progressSEXP);
    rcpp_result_gen = Rcpp::wrap(nneighbors_sphere_rcpp(amat, radius, threads, progress));
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
// rotate2D_rcpp
NumericMatrix rotate2D_rcpp(NumericMatrix plane, NumericVector angle, int threads);
RcppExport SEXP _rTLS_rotate2D_rcpp(SEXP planeSEXP, SEXP angleSEXP, SEXP threadsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type plane(planeSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type angle(angleSEXP);
    Rcpp::traits::input_parameter< int >::type threads(threadsSEXP);
    rcpp_result_gen = Rcpp::wrap(rotate2D_rcpp(plane, angle, threads));
    return rcpp_result_gen;
END_RCPP
}
// rotate3D_rcpp
NumericMatrix rotate3D_rcpp(NumericMatrix cloud, NumericVector roll, NumericVector pitch, NumericVector yaw, int threads);
RcppExport SEXP _rTLS_rotate3D_rcpp(SEXP cloudSEXP, SEXP rollSEXP, SEXP pitchSEXP, SEXP yawSEXP, SEXP threadsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type cloud(cloudSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type roll(rollSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type pitch(pitchSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type yaw(yawSEXP);
    Rcpp::traits::input_parameter< int >::type threads(threadsSEXP);
    rcpp_result_gen = Rcpp::wrap(rotate3D_rcpp(cloud, roll, pitch, yaw, threads));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_rTLS_cartesian_to_polar_rcpp", (DL_FUNC) &_rTLS_cartesian_to_polar_rcpp, 3},
    {"_rTLS_dimensionality_knn_rcpp", (DL_FUNC) &_rTLS_dimensionality_knn_rcpp, 5},
    {"_rTLS_dimensionality_sphere_rcpp", (DL_FUNC) &_rTLS_dimensionality_sphere_rcpp, 5},
    {"_rTLS_euclidean_distance", (DL_FUNC) &_rTLS_euclidean_distance, 3},
    {"_rTLS_meanDis_knn_rcpp", (DL_FUNC) &_rTLS_meanDis_knn_rcpp, 4},
    {"_rTLS_minimun_distance_rcpp", (DL_FUNC) &_rTLS_minimun_distance_rcpp, 3},
    {"_rTLS_nneighbors_sphere_rcpp", (DL_FUNC) &_rTLS_nneighbors_sphere_rcpp, 4},
    {"_rTLS_polar_to_cartesian_rcpp", (DL_FUNC) &_rTLS_polar_to_cartesian_rcpp, 2},
    {"_rTLS_rotate2D_rcpp", (DL_FUNC) &_rTLS_rotate2D_rcpp, 3},
    {"_rTLS_rotate3D_rcpp", (DL_FUNC) &_rTLS_rotate3D_rcpp, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_rTLS(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
