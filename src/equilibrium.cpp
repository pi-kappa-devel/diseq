#include <Rcpp.h>

#include "gsl/gsl_errno.h"
#include "gsl/gsl_multimin.h"
#include "gsl/gsl_vector.h"

#if __cplusplus > 201703L && !defined(__MINGW32__)
#define _DISEQ_HAS_EXECUTION_POLICIES_ 1
#else
#define _DISEQ_HAS_EXECUTION_POLICIES_ 0
#endif

#if _DISEQ_HAS_EXECUTION_POLICIES_
#include <execution>
#endif
#include <numeric>

static gsl_error_handler_t *defaul_gsl_error_handler = gsl_set_error_handler_off();

class equilibrium_model {
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

  size_t demand_independent_variables_size;
  size_t demand_price_variable_size;
  size_t demand_control_variables_size;
  size_t demand_variance_variable_size;
  size_t supply_independent_variables_size;
  size_t supply_price_variable_size;
  size_t supply_control_variables_size;
  size_t supply_variance_variable_size;
  size_t correlation_variable_size;
  size_t independent_variable_size;
  size_t gradient_size;

  gsl_vector *alphad_betad;
  gsl_vector *alphas_betas;
  double alphad;
  double alphas;
  gsl_vector *betad;
  gsl_vector *betas;
  double delta;
  double sigmad;
  double sigmas;
  double rho;
  double rho1;
  double rho2;

  std::vector<std::vector<double>> Xd;
  std::vector<std::vector<double>> Xs;
  std::vector<double> P;
  std::vector<double> Q;

  std::vector<double> mu_P;
  std::vector<double> mu_Q;

  double sigma_P;
  double sigma_Q;
  double cov_QP;
  double rho_QP;
  double rho1_QP;
  double rho2_QP;

  std::vector<double> h_P;
  std::vector<double> h_Q;

  std::vector<double> z_PQ;
  std::vector<double> z_QP;

  std::vector<double> llh;
  double sum_llh;

  std::vector<double> partial_alpha_d;
  std::vector<std::vector<double>> partial_beta_d;
  std::vector<double> partial_alpha_s;
  std::vector<std::vector<double>> partial_beta_s;
  std::vector<double> partial_var_d;
  std::vector<double> partial_var_s;
  std::vector<double> partial_rho;

  double rho_sigmad_sigmas;
  double sigmad2;
  double sigmas2;

  double delta2;
  double sigma_P2;
  double sigma_Q2;
  double sigma_P_sigma_Q;
  double rho1_QP2;
  double rho2_QP2;

  std::vector<double> Xdbetad;
  std::vector<double> Xsbetas;
  std::vector<double> h_P2;
  std::vector<double> h_Q2;

  bool has_correlated_shocks{false};

  std::vector<size_t> row_indices;
  std::vector<size_t> col_indices;

  equilibrium_model(Rcpp::S4 system) {
    Rcpp::Environment diseq = Rcpp::Environment::namespace_env("diseq");
    Rcpp::S4 demand = system.slot("demand");
    Rcpp::S4 supply = system.slot("supply");

    Rcpp::Function get_prefixed_independent_variables = diseq["get_prefixed_independent_variables"];
    demand_independent_variables = get_prefixed_independent_variables(demand);
    supply_independent_variables = get_prefixed_independent_variables(supply);

    demand_independent_variables_size = demand_independent_variables.length();
    supply_independent_variables_size = supply_independent_variables.length();

    alphad_betad = gsl_vector_alloc(demand_independent_variables_size);
    alphas_betas = gsl_vector_alloc(supply_independent_variables_size);

    Rcpp::Function get_prefixed_price_variable = diseq["get_prefixed_price_variable"];
    demand_price_variable =
        Rcpp::as<Rcpp::String>(get_prefixed_price_variable(demand)).get_cstring();
    supply_price_variable =
        Rcpp::as<Rcpp::String>(get_prefixed_price_variable(supply)).get_cstring();

    demand_price_variable_size = demand_price_variable.empty() ? 0 : 1;
    supply_price_variable_size = supply_price_variable.empty() ? 0 : 1;

    Rcpp::Function get_prefixed_control_variables = diseq["get_prefixed_control_variables"];
    demand_control_variables = get_prefixed_control_variables(demand);
    supply_control_variables = get_prefixed_control_variables(supply);

    demand_control_variables_size = demand_control_variables.length();
    supply_control_variables_size = supply_control_variables.length();

    betad = gsl_vector_alloc(demand_control_variables_size);
    betas = gsl_vector_alloc(supply_control_variables_size);

    Rcpp::Function get_prefixed_variance_variable = diseq["get_prefixed_variance_variable"];
    demand_variance_variable =
        Rcpp::as<Rcpp::String>(get_prefixed_variance_variable(demand)).get_cstring();
    supply_variance_variable =
        Rcpp::as<Rcpp::String>(get_prefixed_variance_variable(supply)).get_cstring();

    demand_variance_variable_size = 1;
    supply_variance_variable_size = 1;

    Rcpp::Function get_correlation_variable = diseq["get_correlation_variable"];
    correlation_variable = Rcpp::as<Rcpp::String>(get_correlation_variable(system)).get_cstring();
    has_correlated_shocks = Rcpp::as<bool>(system.slot("correlated_shocks"));

    correlation_variable_size = has_correlated_shocks ? 1 : 0;

    Rcpp::NumericMatrix buffer = demand.slot("control_matrix");
    Xd.reserve(buffer.nrow());
    for (int r = 0; r < buffer.nrow(); ++r) {
      Rcpp::NumericVector buf = buffer(r, Rcpp::_);
      Xd.push_back({buf.begin(), buf.end()});
    }
    buffer = Rcpp::as<Rcpp::NumericMatrix>(supply.slot("control_matrix"));
    Xs.reserve(buffer.nrow());
    for (int r = 0; r < buffer.nrow(); ++r) {
      Rcpp::NumericVector buf = buffer(r, Rcpp::_);
      Xs.push_back({buf.begin(), buf.end()});
    }

    buffer = Rcpp::as<Rcpp::NumericMatrix>(system.slot("price_vector"));
    P.assign(buffer.begin(), buffer.end());
    buffer = Rcpp::as<Rcpp::NumericMatrix>(system.slot("quantity_vector"));
    Q.assign(buffer.begin(), buffer.end());

    mu_P.assign(Xd.size(), 0.0);
    mu_Q.assign(Xd.size(), 0.0);

    h_P.assign(Xd.size(), 0.0);
    h_Q.assign(Xd.size(), 0.0);

    z_PQ.assign(Xd.size(), 0.0);
    z_QP.assign(Xd.size(), 0.0);

    llh.assign(Xd.size(), 0.0);

    partial_alpha_d.assign(Xd.size(), 0.0);
    partial_beta_d.assign(Xd[0].size(), {});
    for (auto &row : partial_beta_d) {
      row.assign(Xd.size(), 0.0);
    }
    partial_alpha_s.assign(Xd.size(), 0.0);
    partial_beta_s.assign(Xs[0].size(), {});
    for (auto &row : partial_beta_s) {
      row.assign(Xd.size(), 0.0);
    }
    partial_var_d.assign(Xd.size(), 0.0);
    partial_var_s.assign(Xd.size(), 0.0);
    partial_rho.assign(Xd.size(), 0.0);

    Xdbetad.assign(Xd.size(), 0.0);
    Xsbetas.assign(Xd.size(), 0.0);
    h_P2.assign(Xd.size(), 0.0);
    h_Q2.assign(Xd.size(), 0.0);

    independent_variable_size =
        demand_independent_variables_size + supply_independent_variables_size;
    gradient_size = independent_variable_size + demand_variance_variable_size +
                    supply_variance_variable_size + correlation_variable_size;

    row_indices.assign(Xd.size(), 0);
    std::iota(row_indices.begin(), row_indices.end(), 0);
    col_indices.assign(gradient_size, 0);
    std::iota(col_indices.begin(), col_indices.end(), 0);
  }

  ~equilibrium_model() {
    gsl_vector_free(alphad_betad);
    gsl_vector_free(alphas_betas);

    gsl_vector_free(betad);
    gsl_vector_free(betas);
  }

  void system_base_set_parameters(const gsl_vector *v) {
    size_t offsetd = 0;
    size_t offsets = demand_independent_variables_size;
    gsl_vector view_vector;

    view_vector = gsl_vector_const_subvector(v, offsetd, demand_independent_variables_size).vector;
    gsl_vector_memcpy(alphad_betad, &view_vector);
    view_vector = gsl_vector_const_subvector(v, offsets, supply_independent_variables_size).vector;
    gsl_vector_memcpy(alphas_betas, &view_vector);

    if (demand_price_variable_size) {
      alphad = gsl_vector_get(v, offsetd);
      ++offsetd;
    } else {
      alphad = GSL_NAN;
    }
    if (supply_price_variable_size) {
      alphas = gsl_vector_get(v, offsets);
      ++offsets;
    } else {
      alphas = GSL_NAN;
    }
    delta = alphas - alphad;

    view_vector = gsl_vector_const_subvector(v, offsetd, demand_control_variables_size).vector;
    gsl_vector_memcpy(betad, &view_vector);
    view_vector = gsl_vector_const_subvector(v, offsets, supply_control_variables_size).vector;
    gsl_vector_memcpy(betas, &view_vector);

    offsets += supply_control_variables_size;
    offsetd = offsets++;

    double vard = gsl_vector_get(v, offsetd);
    if (vard < 0) {
      sigmad = GSL_NAN;
    } else {
      sigmad = std::sqrt(vard);
    }
    double vars = gsl_vector_get(v, offsets++);
    if (vars < 0) {
      sigmas = GSL_NAN;
    } else {
      sigmas = std::sqrt(vars);
    }

    if (has_correlated_shocks) {
      rho = gsl_vector_get(v, offsets);
      if (rho > 1) {
        rho2 = rho1 = rho = GSL_NAN;
      } else {
        rho1 = 1 / std::sqrt(1 - std::pow(rho, 2));
        rho2 = rho * rho1;
      }
    }
  }

  void system_equilibrium_model_set_parameters(const gsl_vector *v) {
    system_base_set_parameters(v);

    rho_sigmad_sigmas = rho * sigmad * sigmas;
    sigmad2 = std::pow(sigmad, 2);
    sigmas2 = std::pow(sigmas, 2);

    double var_P = (-2 * rho_sigmad_sigmas + sigmad2 + sigmas2) / std::pow(-alphad + alphas, 2);
    double var_Q = (std::pow(alphad, 2) * sigmas2 - 2 * alphad * alphas * rho_sigmad_sigmas +
                    std::pow(alphas, 2) * sigmad2) /
                   std::pow(-alphad + alphas, 2);

    sigma_P = std::sqrt(var_P);
    sigma_Q = std::sqrt(var_Q);

    cov_QP = (alphad * sigmas2 + alphas * sigmad2 - rho_sigmad_sigmas * (alphad + alphas)) /
             std::pow(-alphad + alphas, 2);
    rho_QP = cov_QP / sigma_P / sigma_Q;
    rho1_QP = 1 / std::sqrt(1 - std::pow(rho_QP, 2));
    rho2_QP = rho_QP * rho1_QP;
    if (rho_QP == NA_REAL || std::fabs(rho_QP) >= 1) {
      rho_QP = NA_REAL;
      rho1_QP = NA_REAL;
      rho2_QP = NA_REAL;
    }

    delta2 = std::pow(delta, 2);
    sigma_P2 = std::pow(sigma_P, 2);
    sigma_Q2 = std::pow(sigma_Q, 2);
    sigma_P_sigma_Q = sigma_P * sigma_Q;
    rho1_QP2 = std::pow(rho1_QP, 2);
    rho2_QP2 = std::pow(rho2_QP, 2);

    std::for_each(
#if _DISEQ_HAS_EXECUTION_POLICIES_
        std::execution::par_unseq,
#endif
        row_indices.begin(), row_indices.end(), [&](size_t r) {
          Xdbetad[r] = 0;
          for (size_t c = 0; c < Xd[r].size(); ++c) {
            Xdbetad[r] += Xd[r][c] * gsl_vector_get(betad, c);
          }
          Xsbetas[r] = 0;
          for (size_t c = 0; c < Xs[r].size(); ++c) {
            Xsbetas[r] += Xs[r][c] * gsl_vector_get(betas, c);
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

    // Floating point addition is not commutative. For deterministic output the summation has to
    // be performed in a prespecified order.
  sum_llh = std::accumulate(llh.begin(), llh.end(), 0.0);
  }

  void calculate_gradient(gsl_vector *df) {
    std::for_each(
#if _DISEQ_HAS_EXECUTION_POLICIES_
        std::execution::par_unseq,
#endif
        row_indices.begin(), row_indices.end(), [&](size_t r) {
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
          for (size_t c = 0; c < partial_beta_d.size(); ++c) {
            partial_beta_d[c][r] = Xd[r][c] * partial_beta_d_scale;
          }

          double partial_beta_s_scale =
              -rho1_QP * (alphad * sigma_P * z_QPr + sigma_Q * z_PQr) / (delta * sigma_P_sigma_Q);
          for (size_t c = 0; c < partial_beta_s.size(); ++c) {
            partial_beta_s[c][r] = Xs[r][c] * partial_beta_s_scale;
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

    // Floating point addition is not commutative. For deterministic output the summation has to
    // be performed in a prespecified order.
    std::memset(df->data, 0, sizeof(df->data[0]) * df->size);
    for (size_t r = 0; r < partial_alpha_d.size(); ++r) {
      df->data[0] -= partial_alpha_d[r];
      for (size_t c = 0; c < demand_control_variables_size; ++c) {
        df->data[c + 1] -= partial_beta_d[c][r];
      }
      df->data[demand_independent_variables_size] -= partial_alpha_s[r];
      for (size_t c = 0; c < supply_control_variables_size; ++c) {
        df->data[c + demand_independent_variables_size + 1] -= partial_beta_s[c][r];
      }
      df->data[independent_variable_size] -= partial_var_d[r];
      df->data[independent_variable_size + 1] -= partial_var_s[r];
      df->data[independent_variable_size + 2] -= partial_rho[r];
    }
  }
};

double my_f(const gsl_vector *v, void *params) {
  equilibrium_model *obj = static_cast<equilibrium_model *>(params);
  obj->system_equilibrium_model_set_parameters(v);

  return -obj->sum_llh;
}

void my_df(const gsl_vector *v, void *params, gsl_vector *df) {
  equilibrium_model *obj = static_cast<equilibrium_model *>(params);
  obj->system_equilibrium_model_set_parameters(v);
  obj->calculate_gradient(df);
}

void my_fdf(const gsl_vector *v, void *params, double *f, gsl_vector *df) {
  equilibrium_model *obj = static_cast<equilibrium_model *>(params);
  obj->system_equilibrium_model_set_parameters(v);

  *f = -obj->sum_llh;
  obj->calculate_gradient(df);
}

std::vector<double> test_df(const gsl_vector *x, double step, void *params) {
  double fx;
  gsl_vector *dfx = gsl_vector_alloc(x->size);
  my_fdf(x, params, &fx, dfx);

  double g = 0;
  for (size_t j = 0; j < x->size; ++j) {
    g += gsl_vector_get(dfx, j) * step;
  }

  std::vector<double> qs(x->size);
  gsl_vector *y = gsl_vector_alloc(x->size);
  for (int i = 0; i < 40; ++i) {
    double s = std::pow(10.0, 20.0 - i);
    for (size_t j = 0; j < y->size; ++j) {
      gsl_vector_set(y, j, gsl_vector_get(x, j) + step * s);
    }
    double fy = my_f(y, params);
    qs.push_back((fy - fx) / g / s);
  }

  return qs;
}

Rcpp::List minimize(equilibrium_model *model, Rcpp::NumericVector &start, double step,
                    double objective_tolerance, double gradient_tolerance) {
  size_t iter = 0;
  int status;

  const gsl_multimin_fdfminimizer_type *T;
  gsl_multimin_fdfminimizer *s;

  gsl_vector *x;
  gsl_multimin_function_fdf my_func;

  my_func.n = start.length();
  my_func.f = my_f;
  my_func.df = my_df;
  my_func.fdf = my_fdf;
  my_func.params = model;

  x = gsl_vector_alloc(start.length());
  for (R_xlen_t i = 0; i < start.length(); ++i) {
    gsl_vector_set(x, i, start[i]);
  }

  T = gsl_multimin_fdfminimizer_vector_bfgs2;
  s = gsl_multimin_fdfminimizer_alloc(T, start.length());

  gsl_multimin_fdfminimizer_set(s, &my_func, x, step, objective_tolerance);

  do {
    iter++;
    status = gsl_multimin_fdfminimizer_iterate(s);

    if (status) {
      break;
    }

    status = gsl_multimin_test_gradient(s->gradient, gradient_tolerance);
  } while (status == GSL_CONTINUE && iter < 1e+5);

  Rcpp::NumericVector optimizer(s->x->size);
  Rcpp::NumericVector gradient(s->gradient->size);
  double log_likelihood = s->f;
  std::for_each(
#if _DISEQ_HAS_EXECUTION_POLICIES_
      std::execution::par_unseq,
#endif
      model->col_indices.begin(), model->col_indices.end(), [&](size_t c) {
        optimizer[c] = s->x->data[c];
        gradient[c] = s->gradient->data[c];
      });

  gsl_multimin_fdfminimizer_free(s);
  gsl_vector_free(x);

  return Rcpp::List::create(
      Rcpp::_["step"] = step, Rcpp::_["objective_tolerance"] = objective_tolerance,
      Rcpp::_["gradient_tolerance"] = gradient_tolerance, Rcpp::_["status"] = status,
      Rcpp::_["optimizer"] = optimizer, Rcpp::_["gradient"] = gradient,
      Rcpp::_["log_likelihood"] = log_likelihood, Rcpp::_["iterations"] = iter);
}

RCPP_MODULE(diseq_module) {
  Rcpp::class_<equilibrium_model>("cpp_equilibrium_model")
      .constructor<Rcpp::S4>()
      .method("minimize", &minimize);
}
