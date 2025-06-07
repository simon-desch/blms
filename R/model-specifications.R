## model funcs -------------------------------------------------------------


### hmm_rp_model_func -------------------------------------------------------


hmm_rp_model_func <-
  brms::stanvar(
    scode = '
    vector hmm_rp_probs(vector choice, vector reward, vector block_grp, vector gamma_vec, vector c_vec, vector d_vec) {


    int nT = size(choice);

    vector[nT] gamma;
    vector[nT] c;
    vector[nT] d;
    vector[nT] Ps_out;

    vector[2] Ps;  // prob of the states, 1 - left, 2 - right
    real P_O_S1;   // p(O|S1) - O = {A,R} given left
    real P_O_S2;   // p(O|S2) - O = {A,R} given right

    gamma = gamma_vec;
    c = c_vec;
    d = d_vec;

    vector[2] Ps_init; // initial prob/belief of the two states
    Ps_init = rep_vector(0.5, 2);

    for (n in 1:nT)  {

      // State update using the transition matrix
      // from S[t-1] to S[t], BEFORE observing the outcome
      if (n == 1 || block_grp[n]!=block_grp[n-1]) {
        Ps = Ps_init;
      } else {
         Ps[1] = Ps[1] * (1-gamma[n]) + Ps[2] * gamma[n];
         Ps[2] = 1 - Ps[1];
      }

      // action selection based on the State probability
      Ps_out[n] = Ps[2]; // if choice is coded as [1, 2], then level two (2) is used as upper bound in bernoulli, so we need p(S2)
      // if (only_prior==1) {
      //   choice[s,cc,t] ~ bernoulli(Ps);
      // }

      // renew of emission prob: p(O|S1) p(O|S2); O is a pair between A(ction) and R(eward)
      // --> the probability of actually observing this outcome
      if (reward[n] == 1) {
        P_O_S1 = 0.5 * ( (choice[n] == 1)?c[n]:(1-c[n]) );
        P_O_S2 = 0.5 * ( (choice[n] == 2)?c[n]:(1-c[n]) );
      } else if (reward[n] == -1) {
        P_O_S1 = 0.5 * ( (choice[n] == 1)?(1-d[n]):d[n] );
        P_O_S2 = 0.5 * ( (choice[n] == 2)?(1-d[n]):d[n] );
      }

      // State belief update using Bayesian rule, after observing the outcome
      if (( P_O_S1 * Ps[1] + P_O_S2 * Ps[2]) != 0) {
        Ps[1] = (P_O_S1 * Ps[1]) / ( P_O_S1 * Ps[1] + P_O_S2 * Ps[2]);
        Ps[2] = 1 - Ps[1];
      }
    } // trial loop

    return Ps_out;
  }
    ',
    block = 'functions'
  )


### hmm_rp_model_func2 ------------------------------------------------------


hmm_rp_model_func2 <-
  brms::stanvar(
    scode = '
  vector hmm_rp_probs(vector choice, vector reward, vector gamma, vector c, vector d) {
    int nT = size(choice);

    real P_O_S1;   // p(O|S1) - O = {A,R} given left
    real P_O_S2;   // p(O|S2) - O = {A,R} given right

    vector[2] Ps;  // prob of the states, 1 - left, 2 - right
    vector[2] Ps_init; // initial prob/belief of the two states
    vector[nT] Ps_out;
    Ps_init = rep_vector(0.5, 2);
    Ps = Ps_init;

    for (n in 1:nT)  {

      // State update using the transition matrix
      // from S[t-1] to S[t], BEFORE observing the outcome
      if (n > 1) {
         Ps[1] = Ps[1] * (1-gamma[n]) + Ps[2] * gamma[n];
         Ps[2] = 1 - Ps[1];
      }

      // action selection based on the State probability
      Ps_out[n] = Ps[2]; // if choice is coded as [1, 2], then level two (2) is used as upper bound in bernoulli, so we need p(S2)

      // renew of emission prob: p(O|S1) p(O|S2); O is a pair between A(ction) and R(eward)
      // --> the probability of actually observing this outcome
      if (reward[n] == 1) {
        P_O_S1 = 0.5 * ( (choice[n] == 1)?c[n]:(1-c[n]) );
        P_O_S2 = 0.5 * ( (choice[n] == 2)?c[n]:(1-c[n]) );
      } else if (reward[n] == -1) {
        P_O_S1 = 0.5 * ( (choice[n] == 1)?(1-d[n]):d[n] );
        P_O_S2 = 0.5 * ( (choice[n] == 2)?(1-d[n]):d[n] );
      }

      // State belief update using Bayesian rule, after observing the outcome
      if (( P_O_S1 * Ps[1] + P_O_S2 * Ps[2]) != 0) {
        Ps[1] = (P_O_S1 * Ps[1]) / ( P_O_S1 * Ps[1] + P_O_S2 * Ps[2]);
        Ps[2] = 1 - Ps[1];
      }
    } // trial loop

    return Ps_out;
  }
    ',
    block = 'functions'
  )


### ql_2a_it_model_func -----------------------------------------------------


ql_2a_it_model_func <-
  brms::stanvar(
    scode = '
  vector ql_2a_it_probs(vector choice, vector reward, vector block_grp, vector alphapos, vector alphaneg, vector tau) {
    int nT = size(choice);
    vector[nT] Ps_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real PE;            // prediction error
    real alpha;         // effective learning rate (alphapos or alphaneg, respectively)
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "upper bound" (coded as 1)
                        // 2 corresponds to the "lower bound" (coded as 0)

    Qs_init = rep_vector(0.0, 2);
    Qs = Qs_init;

    for (n in 1:nT)  {

      if (n == 1 || block_grp[n]!=block_grp[n-1]) {
        Qs = Qs_init;
      }

      // get choice of current trial (either 1 or 2)
      ci = (choice[n]==1 ? 2 : 1);

      // compute action probabilities
      Ps = softmax(Qs*tau[n]);
      Ps_out[n] = Ps[2]; // predicting upper bound

      // prediction error
      PE = (reward[n]) - Qs[ci];

      // value updating (learning)
      alpha = (PE >= 0) ? alphapos[n] : alphaneg[n];
      Qs[ci] += alpha * PE;

    } // trial loop

    return Ps_out;
  }
    ',
    block = 'functions'
  )


### ql_a_it_model_func -----------------------------------------------------


ql_a_it_model_func <-
  brms::stanvar(
    scode = '
  vector ql_a_it_probs(vector choice, vector reward, vector block_grp, vector alpha, vector tau) {
    int nT = size(choice);
    vector[nT] Ps_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real PE;            // prediction error
    real alpha;         // effective learning rate (alphapos or alphaneg, respectively)
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "upper bound" (coded as 1)
                        // 2 corresponds to the "lower bound" (coded as 0)

    Qs_init = rep_vector(0.0, 2);
    Qs = Qs_init;

    for (n in 1:nT)  {

      if (n == 1 || block_grp[n]!=block_grp[n-1]) {
        Qs = Qs_init;
      }

      // get choice of current trial (either 1 or 2)
      ci = (choice[n]==1 ? 2 : 1);

      // compute action probabilities
      Ps = softmax(Qs*tau[n]);
      Ps_out[n] = Ps[2]; // predicting upper bound

      // prediction error
      PE = (reward[n]) - Qs[ci];

      // value updating (learning)
      Qs[ci] += alpha * PE;

    } // trial loop

    return Ps_out;
  }
    ',
    block = 'functions'
  )


### ql_2a_xi_model_func -----------------------------------------------------

ql_2a_xi_model_func <-
  brms::stanvar(
    scode = '
  vector ql_2a_xi_probs(vector choice, vector reward, vector block_grp, vector alphapos, vector alphaneg, vector xi) {
    int nT = size(choice);
    vector[nT] Ps_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real PE;            // prediction error
    real alpha;         // effective learning rate (alphapos or alphaneg, respectively)
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "upper bound" (coded as 1)
                        // 2 corresponds to the "lower bound" (coded as 0)

    Qs_init = rep_vector(0.0, 2);
    Qs = Qs_init;

    for (n in 1:nT) {

      if (n == 1 || block_grp[n]!=block_grp[n-1]) {
        Qs = Qs_init;
      }

      // get choice of current trial (either 1 or 2)
      ci = (choice[n]==1 ? 2 : 1);

      // compute action probabilities
      Ps = softmax(Qs);
      Ps_out[n] = Ps[2] * (1-xi[n]) + xi[n]/2; // predicting upper bound

      // prediction error
      PE = (reward[n]) - Qs[ci];

      // value updating (learning)
      alpha = (PE >= 0) ? alphapos[n] : alphaneg[n];
      Qs[ci] += alpha * PE;

    } // trial loop

    return Ps_out;
  }
    ',
    block = 'functions'
  )

### ql_a_xi_model_func -----------------------------------------------------

ql_a_xi_model_func <-
  brms::stanvar(
    scode = '
  vector ql_a_xi_probs(vector choice, vector reward, vector block_grp, vector alpha, vector xi) {
    int nT = size(choice);
    vector[nT] Ps_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real PE;            // prediction error
    real alpha;         // effective learning rate (alphapos or alphaneg, respectively)
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "upper bound" (coded as 1)
                        // 2 corresponds to the "lower bound" (coded as 0)

    Qs_init = rep_vector(0.0, 2);
    Qs = Qs_init;

    for (n in 1:nT) {

      if (n == 1 || block_grp[n]!=block_grp[n-1]) {
        Qs = Qs_init;
      }

      // get choice of current trial (either 1 or 2)
      ci = (choice[n]==1 ? 2 : 1);

      // compute action probabilities
      Ps = softmax(Qs);
      Ps_out[n] = Ps[2] * (1-xi[n]) + xi[n]/2; // predicting upper bound

      // prediction error
      PE = (reward[n]) - Qs[ci];

      // value updating (learning)
      Qs[ci] += alpha * PE;

    } // trial loop

    return Ps_out;
  }
    ',
    block = 'functions'
  )

### ql_2a_rho_model_func -----------------------------------------------------

ql_2a_rho_model_func <-
  brms::stanvar(
    scode = '
  vector ql_2a_rho_probs(vector choice, vector reward, vector block_grp, vector alphapos, vector alphaneg, vector rho) {
    int nT = size(choice);
    vector[nT] Ps_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real PE;            // prediction error
    real alpha;         // effective learning rate (alphapos or alphaneg, respectively)
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "upper bound" (coded as 1)
                        // 2 corresponds to the "lower bound" (coded as 0)

    Qs_init = rep_vector(0.0, 2);
    Qs = Qs_init;

    for (n in 1:nT) {

      if (n == 1 || block_grp[n]!=block_grp[n-1]) {
        Qs = Qs_init;
      }

      // get choice of current trial (either 1 or 2)
      ci = (choice[n]==1 ? 2 : 1);

      // compute action probabilities
      Ps = softmax(Qs);
      Ps_out[n] = Ps[2]; // predicting upper bound

      // prediction error
      PE = (rho[n]*reward[n]) - Qs[ci];

      // value updating (learning)
      alpha = (PE >= 0) ? alphapos[n] : alphaneg[n];
      Qs[ci] += alpha * PE;

    } // trial loop

    return Ps_out;
  }
    ',
    block = 'functions'
  )

### ql_a_rho_model_func -----------------------------------------------------

ql_a_rho_model_func <-
  brms::stanvar(
    scode = '
  vector ql_a_rho_probs(vector choice, vector reward, vector block_grp, vector alpha, vector rho) {
    int nT = size(choice);
    vector[nT] Ps_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real PE;            // prediction error
    real alpha;         // effective learning rate (alphapos or alphaneg, respectively)
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "upper bound" (coded as 1)
                        // 2 corresponds to the "lower bound" (coded as 0)

    Qs_init = rep_vector(0.0, 2);
    Qs = Qs_init;

    for (n in 1:nT) {

      if (n == 1 || block_grp[n]!=block_grp[n-1]) {
        Qs = Qs_init;
      }

      // get choice of current trial (either 1 or 2)
      ci = (choice[n]==1 ? 2 : 1);

      // compute action probabilities
      Ps = softmax(Qs);
      Ps_out[n] = Ps[2]; // predicting upper bound

      // prediction error
      PE = (rho[n]*reward[n]) - Qs[ci];

      // value updating (learning)
      Qs[ci] += alpha * PE;

    } // trial loop

    return Ps_out;
  }
    ',
    block = 'functions'
  )

### ql_2a_2rho_model_func -----------------------------------------------------

ql_2a_2rho_model_func <-
  brms::stanvar(
    scode = '
  vector ql_2a_2rho_probs(vector choice, vector reward, vector block_grp, vector alphapos, vector alphaneg, vector rhopos, vector rhoneg) {
    int nT = size(choice);
    vector[nT] Ps_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real PE;            // prediction error
    real alpha;         // effective learning rate (alphapos or alphaneg, respectively)
    real rho;           // effective reward sensitivity (rhopos or rhoneg, respectively)
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "upper bound" (coded as 1)
                        // 2 corresponds to the "lower bound" (coded as 0)

    Qs_init = rep_vector(0.0, 2);
    Qs = Qs_init;

    for (n in 1:nT) {

      if (n == 1 || block_grp[n]!=block_grp[n-1]) {
        Qs = Qs_init;
      }

      // get choice of current trial (either 1 or 2)
      ci = (choice[n]==1 ? 2 : 1);

      // compute action probabilities
      Ps = softmax(Qs);
      Ps_out[n] = Ps[2]; // predicting upper bound

      // prediction error
      rho = (reward[n] > 0 ? rhopos[n] : rhoneg[n]);
      PE = (rho*reward[n]) - Qs[ci];

      // value updating (learning)
      alpha = (PE >= 0) ? alphapos[n] : alphaneg[n];
      Qs[ci] += alpha * PE;

    } // trial loop

    return Ps_out;
  }
    ',
    block = 'functions'
  )

### ql_a_2rho_model_func -----------------------------------------------------

ql_a_2rho_model_func <-
  brms::stanvar(
    scode = '
  vector ql_a_rho_probs(vector choice, vector reward, vector block_grp, vector alpha, vector rhopos, vector rhoneg) {
    int nT = size(choice);
    vector[nT] Ps_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real PE;            // prediction error
    real alpha;         // effective learning rate (alphapos or alphaneg, respectively)
    real alpha;         // effective reward sensitivity (alphapos or alphaneg, respectively)
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "upper bound" (coded as 1)
                        // 2 corresponds to the "lower bound" (coded as 0)

    Qs_init = rep_vector(0.0, 2);
    Qs = Qs_init;

    for (n in 1:nT) {

      if (n == 1 || block_grp[n]!=block_grp[n-1]) {
        Qs = Qs_init;
      }

      // get choice of current trial (either 1 or 2)
      ci = (choice[n]==1 ? 2 : 1);

      // compute action probabilities
      Ps = softmax(Qs);
      Ps_out[n] = Ps[2]; // predicting upper bound

      // prediction error
      PE = (rho[n]*reward[n]) - Qs[ci];

      // value updating (learning)
      Qs[ci] += alpha * PE;

    } // trial loop

    return Ps_out;
  }
    ',
    block = 'functions'
  )

## model specs -------------------------------------------------------------


#' Get currently available model specifications
#'
#' @returns a `list()` of model specifications for blms models
#' @export
get_all_model_specs <-
  function() {
    model_specs <-
      list(
        hmm_rp =
          list(
            class = 'hmm_rp',
            description = 'HMM model with separate emission probabilities for reward and punishment outcomes (cf. Schlagenhauf et al. 2014)',
            parameters = list(
              gamma = list(lb = 0, ub = 1),
              c = list(lb = 0.5, ub = 1),
              d = list(lb = 0.5, ub = 1)
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            block_var = 'blockgrp',
            func_name = 'hmm_rp_probs',
            func_stanvar = hmm_rp_model_func,
            family =  brms::bernoulli(link='identity'),
            par_form = list(gamma ~1, c ~1, d ~1),
            par_transform =
              list(gamma ~ inv_logit(gamma),
                   c + d ~ inv_logit(x) * 0.5 + 0.5)
          ),
        hmm_rp2 =
          list(
            class = 'hmm_rp2',
            description = 'HMM model with separate emission probabilities for reward and punishment outcomes (cf. Schlagenhauf et al. 2014)',
            parameters = list(
              gamma = list(lb = 0, ub = 1),
              c = list(lb = 0.5, ub = 1),
              d = list(lb = 0.5, ub = 1)
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            block_var = NULL,
            func_name = 'hmm_rp_probs',
            func_stanvar = hmm_rp_model_func2,
            family =  brms::bernoulli(link='identity'),
            par_form = list(gamma ~1, c ~1, d ~1),
            par_transform =
              list(gamma ~ inv_logit(gamma),
                   c + d ~ inv_logit(x) * 0.5 + 0.5)
          ),
        ql_a_it =
          list(
            class = 'ql_a_it',
            description = 'Rescorla-Wagner delta learning model with single learning rate and inverse temperature',
            parameters = list(
              alpha = list(lb = 0, ub = 1),
              tau = list(lb = 0.0, ub = Inf)
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            block_var = 'blockgrp',
            func_name = 'ql_a_it_probs',
            func_stanvar = ql_a_it_model_func,
            family =  brms::bernoulli(link='identity'),
            par_form = list(alpha ~1, tau ~1),
            par_transform =
              list(alpha ~ inv_logit(alpha),
                   tau ~ inv_logit(tau) * 20.0)
          ),
        ql_2a_it =
          list(
            class = 'ql_2a_it',
            description = 'Rescorla-Wagner delta learning model with separate learning rates for positive and negative prediction errors and inverse temperature',
            parameters = list(
              alphapos = list(lb = 0, ub = 1),
              alphaneg = list(lb = 0, ub = 1),
              tau = list(lb = 0.0, ub = Inf)
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            block_var = 'blockgrp',
            func_name = 'ql_2a_it_probs',
            func_stanvar = ql_2a_it_model_func,
            family =  brms::bernoulli(link='identity'),
            par_form = list(alphapos ~1, alphaneg ~1, tau ~1),
            par_transform =
              list(alphapos + alphaneg ~ inv_logit(x),
                   tau ~ inv_logit(tau) * 20.0)
          ),
        ql_a_xi =
          list(
            class = 'ql_a_xi',
            description = 'Rescorla-Wagner delta learning model with single learning rate and lapse parameter',
            parameters = list(
              alpha = list(lb = 0, ub = 1),
              xi = list(lb = 0.0, ub = Inf)
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            block_var = 'blockgrp',
            func_name = 'ql_a_xi_probs',
            func_stanvar = ql_a_xi_model_func,
            family =  brms::bernoulli(link='identity'),
            par_form = list(alpha ~1, xi ~1),
            par_transform =
              list(alpha ~ inv_logit(alpha),
                   xi ~ inv_logit(xi))
          ),
        ql_2a_xi =
          list(
            class = 'ql_2a_xi',
            description = 'Rescorla-Wagner delta learning model with separate learning rates for positive and negative prediction errors and lapse parameter',
            parameters = list(
              alphapos = list(lb = 0, ub = 1),
              alphaneg = list(lb = 0, ub = 1),
              xi = list(lb = 0.0, ub = Inf)
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            block_var = 'blockgrp',
            func_name = 'ql_2a_xi_probs',
            func_stanvar = ql_2a_xi_model_func,
            family =  brms::bernoulli(link='identity'),
            par_form = list(alphapos ~1, alphaneg ~1, xi ~1),
            par_transform =
              list(alphapos + alphaneg ~ inv_logit(x),
                   xi ~ inv_logit(xi))
          ),
        ql_a_rho =
          list(
            class = 'ql_a_rho',
            description = 'Rescorla-Wagner delta learning model with single learning rate and reward sensitivity parameter',
            parameters = list(
              alpha = list(lb = 0, ub = 1),
              rho = list(lb = -Inf, ub = Inf)
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            block_var = 'blockgrp',
            func_name = 'ql_a_rho_probs',
            func_stanvar = ql_a_rho_model_func,
            family =  brms::bernoulli(link='identity'),
            par_form = list(alpha ~1, rho ~1),
            par_transform =
              list(alpha ~ inv_logit(alpha))
          ),
        ql_2a_rho =
          list(
            class = 'ql_2a_rho',
            description = 'Rescorla-Wagner delta learning model with separate learning rates for positive and negative prediction errors and reward sensitivity parameter',
            parameters = list(
              alphapos = list(lb = 0, ub = 1),
              alphaneg = list(lb = 0, ub = 1),
              rho = list(lb = -Inf, ub = Inf)
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            block_var = 'blockgrp',
            func_name = 'ql_2a_rho_probs',
            func_stanvar = ql_2a_rho_model_func,
            family =  brms::bernoulli(link='identity'),
            par_form = list(alphapos ~ 1, alphaneg ~ 1, rho ~ 1),
            par_transform =
              list(alphapos + alphaneg ~ inv_logit(x))
          ),
        ql_a_2rho =
          list(
            class = 'ql_a_2rho',
            description = 'Rescorla-Wagner delta learning model with single learning rate and separate reward sensitivity parameters for positive and negative outcomes',
            parameters = list(
              alpha = list(lb = 0, ub = 1),
              rhopos = list(lb = -Inf, ub = Inf),
              rhoneg = list(lb = -Inf, ub = Inf)
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            block_var = 'blockgrp',
            func_name = 'ql_a_2rho_probs',
            func_stanvar = ql_a_2rho_model_func,
            family =  brms::bernoulli(link='identity'),
            par_form = list(alpha ~1, rhopos ~1, rhoneg ~1),
            par_transform =
              list(alpha ~ inv_logit(alpha))
          ),
        ql_2a_2rho =
          list(
            class = 'ql_2a_2rho',
            description = 'Rescorla-Wagner delta learning model with separate learning rates for positive and negative prediction errors and separate reward sensitivity parameters for positive and negative outcomes',
            parameters = list(
              alphapos = list(lb = 0, ub = 1),
              alphaneg = list(lb = 0, ub = 1),
              rhopos = list(lb = -Inf, ub = Inf),
              rhoneg = list(lb = -Inf, ub = Inf)
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            block_var = 'blockgrp',
            func_name = 'ql_2a_2rho_probs',
            func_stanvar = ql_2a_2rho_model_func,
            family =  brms::bernoulli(link='identity'),
            par_form = list(alphapos ~ 1, alphaneg ~ 1, rhopos ~ 1, rhoneg ~1),
            par_transform =
              list(alphapos + alphaneg ~ inv_logit(x))
          )


      )
    return(model_specs)
  }

#' Get model specification for a specific model class
#'
#' @param class character givong the name of the model class
#'
#' @returns a model specification for a blms model
#' @export
get_model_spec <-
  function(class) {
    model_specs <- get_all_model_specs()
    if(!class%in%names(model_specs)) {
      stop('Invalid model class \'', class, '\' ',
           '(not found in get_all_model_specs())')
    }
    model_spec <- model_specs[[class]]
    return(model_spec)
  }

#' Get the names of the parameters fitted by a blms model of a specific class
#'
#' @param model_spec model specification as returned by `get_model_spec()` (see
#'        `get_all_model_specs()` for a list of currently defined models)
#'
#' @returns character vector containing the names of the parameters fitted by
#'          a blms model of a specific class
#' @export
get_parameter_names <-
  function(model_spec) {
    names(model_spec$parameters)
  }
