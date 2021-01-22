#include <Rcpp.h>
#include <execution>

class cpp_eq_fiml_impl {
public:
  Rcpp::CharacterVector demand_independent_variables;
  std::string demand_price_variable;
  Rcpp::CharacterVector demand_control_variables;
  std::string demand_variance_variable;

  Rcpp::CharacterVector supply_independent_variables;
  std::string supply_price_variable;
  Rcpp::CharacterVector supply_control_variables;
  std::string supply_variance_variable;
  std::string correlation_variable;

  Rcpp::NumericMatrix alphad_betad;
  Rcpp::NumericMatrix alphas_betas;
  double alphad;
  double alphas;
  Rcpp::NumericMatrix betad;
  Rcpp::NumericMatrix betas;
  double delta;
  double sigmad;
  double sigmas;
  double rho;

  Rcpp::NumericMatrix Xd;
  Rcpp::NumericMatrix Xs;
  Rcpp::NumericMatrix P;
  Rcpp::NumericMatrix Q;

  Rcpp::NumericMatrix mu_P;
  Rcpp::NumericMatrix mu_Q;

  double sigma_P;
  double sigma_Q;
  double cov_QP;
  double rho_QP;
  double rho1_QP;
  double rho2_QP;

  Rcpp::NumericMatrix h_P;
  Rcpp::NumericMatrix h_Q;

  Rcpp::NumericMatrix z_PQ;
  Rcpp::NumericMatrix z_QP;

  Rcpp::NumericMatrix llh;

  Rcpp::NumericMatrix partial_alpha_d;
  Rcpp::NumericMatrix partial_beta_d;
  Rcpp::NumericMatrix partial_alpha_s;
  Rcpp::NumericMatrix partial_beta_s;
  Rcpp::NumericMatrix partial_var_d;
  Rcpp::NumericMatrix partial_var_s;
  Rcpp::NumericMatrix partial_rho;

  double rho_sigmad_sigmas;
  double sigmad2;
  double sigmas2;

  double delta2;
  double sigma_P2;
  double sigma_Q2;
  double sigma_P_sigma_Q;
  double rho1_QP2;
  double rho2_QP2;

  Rcpp::NumericMatrix Xdbetad;
  Rcpp::NumericMatrix Xsbetas;
  Rcpp::NumericVector h_P2;
  Rcpp::NumericVector h_Q2;

  cpp_eq_fiml_impl(Rcpp::S4 system) {
    Rcpp::Environment diseq = Rcpp::Environment::namespace_env("diseq");
    Rcpp::S4 demand = system.slot("demand");
    Rcpp::S4 supply = system.slot("supply");

    Rcpp::Function get_prefixed_independent_variables = diseq["get_prefixed_independent_variables"];
    demand_independent_variables = get_prefixed_independent_variables(demand);
    supply_independent_variables = get_prefixed_independent_variables(supply);

    Rcpp::Function get_prefixed_price_variable = diseq["get_prefixed_price_variable"];
    demand_price_variable =
        Rcpp::as<Rcpp::String>(get_prefixed_price_variable(demand)).get_cstring();
    supply_price_variable =
        Rcpp::as<Rcpp::String>(get_prefixed_price_variable(supply)).get_cstring();

    Rcpp::Function get_prefixed_control_variables = diseq["get_prefixed_control_variables"];
    demand_control_variables = get_prefixed_control_variables(demand);
    supply_control_variables = get_prefixed_control_variables(supply);

    Rcpp::Function get_prefixed_variance_variable = diseq["get_prefixed_variance_variable"];
    demand_variance_variable =
        Rcpp::as<Rcpp::String>(get_prefixed_variance_variable(demand)).get_cstring();
    supply_variance_variable =
        Rcpp::as<Rcpp::String>(get_prefixed_variance_variable(supply)).get_cstring();

    Rcpp::Function get_correlation_variable = diseq["get_correlation_variable"];
    correlation_variable = Rcpp::as<Rcpp::String>(get_correlation_variable(system)).get_cstring();

    Xd = Rcpp::as<Rcpp::NumericMatrix>(demand.slot("control_matrix"));
    Xs = Rcpp::as<Rcpp::NumericMatrix>(supply.slot("control_matrix"));

    P = Rcpp::as<Rcpp::NumericMatrix>(system.slot("price_vector"));
    Q = Rcpp::as<Rcpp::NumericMatrix>(system.slot("quantity_vector"));

    assert(Xd.ncol() == betad.nrow());
    assert(Xs.ncol() == betas.nrow());
    assert(Xd.nrow() == Xs.nrow());

    mu_P = Rcpp::NumericMatrix(Xd.nrow(), 1);
    mu_Q = Rcpp::NumericMatrix(Xd.nrow(), 1);

    h_P = Rcpp::NumericMatrix(Xd.nrow(), 1);
    h_Q = Rcpp::NumericMatrix(Xd.nrow(), 1);

    z_PQ = Rcpp::NumericMatrix(Xd.nrow(), 1);
    z_QP = Rcpp::NumericMatrix(Xd.nrow(), 1);

    llh = Rcpp::NumericMatrix(Xd.nrow(), 1);

    partial_alpha_d = Rcpp::NumericMatrix(Xd.nrow(), 1);
    partial_beta_d = Rcpp::NumericMatrix(Xd.nrow(), Xd.ncol());
    partial_alpha_s = Rcpp::NumericMatrix(Xs.nrow(), 1);
    partial_beta_s = Rcpp::NumericMatrix(Xs.nrow(), Xs.ncol());
    partial_var_d = Rcpp::NumericMatrix(Xd.nrow(), 1);
    partial_var_s = Rcpp::NumericMatrix(Xs.nrow(), 1);
    partial_rho = Rcpp::NumericMatrix(Xd.nrow(), 1);

    Xdbetad = Rcpp::NumericMatrix(Xd.nrow(), 1);
    Xsbetas = Rcpp::NumericMatrix(Xs.nrow(), 1);
    h_P2 = Rcpp::NumericVector(Xd.nrow());
    h_Q2 = Rcpp::NumericVector(Xs.nrow());
  }

  void system_base_set_parameters(Rcpp::S4 &system, const Rcpp::NumericVector &v) {
    Rcpp::S4 demand = system.slot("demand");
    Rcpp::S4 supply = system.slot("supply");

    Rcpp::NumericVector temp = v[this->demand_independent_variables];
    demand.slot("alpha_beta") = alphad_betad = Rcpp::NumericMatrix(temp.length(), 1, temp.begin());
    temp = v[this->supply_independent_variables];
    supply.slot("alpha_beta") = alphas_betas = Rcpp::NumericMatrix(temp.length(), 1, temp.begin());

    if (!this->demand_price_variable.empty()) {
      demand.slot("alpha") = alphad = v[this->demand_price_variable.c_str()];
    } else {
      demand.slot("alpha") = alphad = NA_REAL;
    }
    if (!this->supply_price_variable.empty()) {
      supply.slot("alpha") = alphas = v[this->supply_price_variable.c_str()];
    } else {
      supply.slot("alpha") = alphas = NA_REAL;
    }
    system.slot("delta") = delta = alphas - alphad;

    temp = v[this->demand_control_variables];
    demand.slot("beta") = betad = Rcpp::NumericMatrix(temp.length(), 1, temp.begin());
    temp = v[this->supply_control_variables];
    supply.slot("beta") = betas = Rcpp::NumericMatrix(temp.length(), 1, temp.begin());

    if (Rcpp::as<double>(v[this->demand_variance_variable.c_str()]) < 0) {
      demand.slot("var") = NA_REAL;
      demand.slot("sigma") = sigmad = NA_REAL;
    } else {
      demand.slot("var") = Rcpp::as<double>(v[this->demand_variance_variable]);
      demand.slot("sigma") = sigmad = std::sqrt(Rcpp::as<double>(demand.slot("var")));
    }
    if (Rcpp::as<double>(v[this->supply_variance_variable.c_str()]) < 0) {
      supply.slot("var") = NA_REAL;
      supply.slot("sigma") = sigmas = NA_REAL;
    } else {
      supply.slot("var") = Rcpp::as<double>(v[this->supply_variance_variable]);
      supply.slot("sigma") = sigmas = std::sqrt(Rcpp::as<double>(supply.slot("var")));
    }

    if (Rcpp::as<bool>(system.slot("correlated_shocks"))) {
      if (Rcpp::as<double>(v[this->correlation_variable]) > 1) {
        system.slot("rho2") = system.slot("rho1") = system.slot("rho") = rho = NA_REAL;
      } else {
        system.slot("rho") = rho = Rcpp::as<double>(v[this->correlation_variable]);
        system.slot("rho1") = 1 / std::sqrt(1 - std::pow(rho, 2));
        system.slot("rho2") = rho * Rcpp::as<double>(system.slot("rho1"));
      }
    }
  }

  Rcpp::S4 system_eq_fiml_set_parameters(Rcpp::S4 system, const Rcpp::NumericVector &v) {
    system_base_set_parameters(system, v);

    std::ranges::iota_view row_indices{0, Xd.nrow()};

    rho_sigmad_sigmas = rho * sigmad * sigmas;
    sigmad2 = std::pow(sigmad, 2);
    sigmas2 = std::pow(sigmas, 2);

    system.slot("var_P") =
        (-2 * rho_sigmad_sigmas + sigmad2 + sigmas2) / std::pow(-alphad + alphas, 2);
    system.slot("var_Q") =
        (std::pow(alphad, 2) * sigmas2 - 2 * alphad * alphas * rho_sigmad_sigmas +
         std::pow(alphas, 2) * sigmad2) /
        std::pow(-alphad + alphas, 2);

    sigma_P = std::sqrt(Rcpp::as<double>(system.slot("var_P")));
    system.slot("sigma_P") = sigma_P;
    sigma_Q = std::sqrt(Rcpp::as<double>(system.slot("var_Q")));
    system.slot("sigma_Q") = sigma_Q;

    cov_QP = (alphad * sigmas2 + alphas * sigmad2 - rho_sigmad_sigmas * (alphad + alphas)) /
             std::pow(-alphad + alphas, 2);
    system.slot("cov_QP") = cov_QP;
    rho_QP = cov_QP / sigma_P / sigma_Q;
    rho1_QP = 1 / std::sqrt(1 - std::pow(rho_QP, 2));
    rho2_QP = rho_QP * rho1_QP;
    if (rho_QP == NA_REAL || std::fabs(rho_QP) >= 1) {
      system.slot("rho_QP") = NA_REAL;
      system.slot("rho1_QP") = NA_REAL;
      system.slot("rho2_QP") = NA_REAL;
    } else {
      system.slot("rho_QP") = rho_QP;
      system.slot("rho1_QP") = rho1_QP;
      system.slot("rho2_QP") = rho2_QP;
    }

    delta2 = std::pow(delta, 2);
    sigma_P2 = std::pow(sigma_P, 2);
    sigma_Q2 = std::pow(sigma_Q, 2);
    sigma_P_sigma_Q = sigma_P * sigma_Q;
    rho1_QP2 = std::pow(rho1_QP, 2);
    rho2_QP2 = std::pow(rho2_QP, 2);

    std::for_each(std::execution::par_unseq, row_indices.begin(), row_indices.end(), [&](size_t r) {
      Xdbetad[r] = 0;
      for (int c = 0; c < Xd.ncol(); ++c) {
        Xdbetad[r] += Xd(r, c) * betad(c, 0);
      }
      Xsbetas[r] = 0;
      for (int c = 0; c < Xs.ncol(); ++c) {
        Xsbetas[r] += Xs(r, c) * betas(c, 0);
      }
      mu_P[r] = (Xdbetad[r] - Xsbetas[r]) / delta;
      mu_Q[r] = (Xdbetad[r] * alphas - Xsbetas[r] * alphad) / (-alphad + alphas);
      h_P[r] = (P[r] - mu_P[r]) / sigma_P;
      h_Q[r] = (Q[r] - mu_Q[r]) / sigma_Q;

      z_PQ[r] = rho1_QP * h_P[r] - rho2_QP * h_Q[r];
      z_QP[r] = rho1_QP * h_Q[r] - rho2_QP * h_P[r];

      llh[r] = -std::log(2 * M_PI) - std::log(sigma_P_sigma_Q / rho1_QP) -
               (std::pow(rho1_QP, 2) *
                (std::pow(h_P[r], 2) - 2 * h_P[r] * h_Q[r] * rho_QP + std::pow(h_Q[r], 2))) /
                   2;

      h_P2[r] = std::pow(h_P[r], 2);
      h_Q2[r] = std::pow(h_Q[r], 2);
    });

    system.slot("mu_P") = mu_P;
    system.slot("mu_Q") = mu_Q;
    system.slot("h_P") = h_P;
    system.slot("h_Q") = h_Q;
    system.slot("z_PQ") = z_PQ;
    system.slot("z_QP") = z_QP;
    system.slot("llh") = llh;

    return system;
  }

  Rcpp::S4 gradient(Rcpp::S4 system) {
    std::ranges::iota_view row_indices{0, Xd.nrow()};

    std::for_each(std::execution::par_unseq, row_indices.begin(), row_indices.end(), [&](size_t r) {
      double Xdbetadr = Xdbetad[r];
      double Xsbetasr = Xsbetas[r];
      double mu_Pr = mu_P[r];
      double mu_Qr = mu_Q[r];
      double h_Pr = h_P[r];
      double h_Qr = h_Q[r];
      double z_PQr = z_PQ[r];
      double z_QPr = z_QP[r];
      double h_P2r = h_P2[r];
      double h_Q2r = h_Q2[r];

      partial_alpha_d[r] =
          (delta2 * mu_Pr * rho1_QP * sigma_Q2 * z_PQr -
           delta2 * rho1_QP * sigma_P_sigma_Q * z_QPr * (Xsbetasr - mu_Qr) +
           delta2 * sigma_P * sigma_Q2 * (h_Pr * rho1_QP * z_PQr - 1) +
           delta * sigma_P * (h_Qr * rho1_QP * z_QPr - 1) *
               (alphad * sigmas2 - alphas * rho_sigmad_sigmas + delta * sigma_Q2) -
           rho1_QP *
               (delta * rho_QP * sigma_P *
                    (alphad * sigmas2 - alphas * rho_sigmad_sigmas + 2 * delta * sigma_Q2) +
                sigma_Q * (-2 * alphas * sigmad2 + rho_sigmad_sigmas * (alphad + 3 * alphas) -
                           sigmas2 * (alphad + alphas))) *
               (h_Pr * h_Qr * rho1_QP * (rho1_QP2 + rho2_QP2) -
                rho1_QP2 * rho2_QP * (h_P2r + h_Q2r) + rho2_QP)) /
          (std::pow(delta, 3) * sigma_P * sigma_Q2);

      partial_alpha_s[r] =
          (-delta2 * mu_Pr * rho1_QP * sigma_Q2 * z_PQr +
           delta2 * rho1_QP * sigma_P_sigma_Q * z_QPr * (Xdbetadr - mu_Qr) +
           delta2 * sigma_P * sigma_Q2 * (-h_Pr * rho1_QP * z_PQr + 1) -
           delta * sigma_P * (h_Qr * rho1_QP * z_QPr - 1) *
               (alphad * rho_sigmad_sigmas - alphas * sigmad2 + delta * sigma_Q2) +
           rho1_QP *
               (delta * rho_QP * sigma_P *
                    (alphad * rho_sigmad_sigmas - alphas * sigmad2 + 2 * delta * sigma_Q2) +
                sigma_Q * (-2 * alphad * sigmas2 + rho_sigmad_sigmas * (3 * alphad + alphas) -
                           sigmad2 * (alphad + alphas))) *
               (h_Pr * h_Qr * rho1_QP * (rho1_QP2 + rho2_QP2) -
                rho1_QP2 * rho2_QP * (h_P2r + h_Q2r) + rho2_QP)) /
          (std::pow(delta, 3) * sigma_P * sigma_Q2);

      double partial_beta_d_scale =
          rho1_QP * (alphas * sigma_P * z_QPr + sigma_Q * z_PQr) / (delta * sigma_P_sigma_Q);
      for (int c = 0; c < Xd.ncol(); ++c) {
        partial_beta_d(r, c) = Xd(r, c) * partial_beta_d_scale;
      }

      double partial_beta_s_scale =
          -rho1_QP * (alphad * sigma_P * z_QPr + sigma_Q * z_PQr) / (delta * sigma_P_sigma_Q);
      for (int c = 0; c < Xs.ncol(); ++c) {
        partial_beta_s(r, c) = Xs(r, c) * partial_beta_s_scale;
      }

      partial_var_d[r] =
          (-alphas * sigma_P2 * (alphad * rho * sigmas - alphas * sigmad) *
               (h_Qr * rho1_QP * z_QPr - 1) +
           rho1_QP *
               (rho_QP * (alphas * sigma_P2 * (alphad * rho * sigmas - alphas * sigmad) +
                          sigma_Q2 * (rho * sigmas - sigmad)) +
                sigma_P_sigma_Q * (2 * alphas * sigmad - rho * sigmas * (alphad + alphas))) *
               (h_Pr * h_Qr * rho1_QP * (rho1_QP2 + rho2_QP2) -
                rho1_QP2 * rho2_QP * (h_P2r + h_Q2r) + rho2_QP) -
           sigma_Q2 * (rho * sigmas - sigmad) * (h_Pr * rho1_QP * z_PQr - 1)) /
          (2 * delta2 * sigma_P2 * sigma_Q2 * sigmad);

      partial_var_s[r] =
          (alphad * sigma_P2 * (alphad * sigmas - alphas * rho * sigmad) *
               (h_Qr * rho1_QP * z_QPr - 1) -
           rho1_QP *
               (rho_QP * (alphad * sigma_P2 * (alphad * sigmas - alphas * rho * sigmad) +
                          sigma_Q2 * (-rho * sigmad + sigmas)) +
                sigma_P_sigma_Q * (-2 * alphad * sigmas + rho * sigmad * (alphad + alphas))) *
               (h_Pr * h_Qr * rho1_QP * (rho1_QP2 + rho2_QP2) -
                rho1_QP2 * rho2_QP * (h_P2r + h_Q2r) + rho2_QP) -
           sigma_Q2 * (rho * sigmad - sigmas) * (h_Pr * rho1_QP * z_PQr - 1)) /
          (2 * delta2 * sigma_P2 * sigma_Q2 * sigmas);

      partial_rho[r] = sigmad * sigmas *
                       (-alphad * alphas * sigma_P2 * (h_Qr * rho1_QP * z_QPr - 1) +
                        rho1_QP *
                            (rho_QP * (alphad * alphas * sigma_P2 + sigma_Q2) -
                             sigma_P_sigma_Q * (alphad + alphas)) *
                            (h_Pr * h_Qr * rho1_QP * (rho1_QP2 + rho2_QP2) -
                             rho1_QP2 * rho2_QP * (h_P2r + h_Q2r) + rho2_QP) -
                        sigma_Q2 * (h_Pr * rho1_QP * z_PQr - 1)) /
                       (delta2 * sigma_P2 * sigma_Q2);
    });

    return system;
  }
};

RCPP_MODULE(diseq_module) {
  Rcpp::class_<cpp_eq_fiml_impl>("cpp_eq_fiml_impl")
      .constructor<Rcpp::S4>()
      .method("system_eq_fiml_set_parameters", &cpp_eq_fiml_impl::system_eq_fiml_set_parameters)
      .method("gradient", &cpp_eq_fiml_impl::gradient)
      .field("partial_alpha_d", &cpp_eq_fiml_impl::partial_alpha_d)
      .field("partial_beta_d", &cpp_eq_fiml_impl::partial_beta_d)
      .field("partial_alpha_s", &cpp_eq_fiml_impl::partial_alpha_s)
      .field("partial_beta_s", &cpp_eq_fiml_impl::partial_beta_s)
      .field("partial_var_d", &cpp_eq_fiml_impl::partial_var_d)
      .field("partial_var_s", &cpp_eq_fiml_impl::partial_var_s)
      .field("partial_rho", &cpp_eq_fiml_impl::partial_rho);
}
