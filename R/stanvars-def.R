## model funcs -------------------------------------------------------------


### hmm_rp_probs_func -------------------------------------------------------


hmm_rp_probs_func <-
  brms::stanvar(
    scode = '
    vector hmm_rp_probs(vector block_grp, vector choice, vector reward, vector gamma_vec, vector c_vec, vector d_vec) {


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


### hmm_g_2ep_probs_func -------------------------------------------------------


hmm_g_2ep_probs_func <-
  brms::stanvar(
    scode = '
    vector hmm_g_2ep_probs(vector block_grp, vector choice, vector reward, vector gamma, vector ep_pos, vector ep_neg) {


    int nT = size(choice);

    vector[nT] Ps_out;

    vector[2] Ps;  // prob of the states, 1 - left, 2 - right
    real P_O_S1;   // p(O|S1) - O = {A,R} given left
    real P_O_S2;   // p(O|S2) - O = {A,R} given right
    real choice_ep; // p(A|R)


    vector[2] Ps_init; // initial prob/belief of the two states
    Ps_init = reptor(0.5, 2);

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

      // renew of emission prob: p(O|S1) p(O|S2); O is a pair between A(ction) and R(eward)
      // --> the probability of actually observing this outcome
      choice_ep = reward[n] >= 0 ? ep_pos[n] : (1-ep_neg[n]);
      P_O_S1 = 0.5 * ( (choice[n] == 1) ? choice_ep : (1-choice_ep) );
      P_O_S2 = 0.5 * ( (choice[n] == 2) ? choice_ep : (1-choice_ep) );

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


### hmm_g_ep_probs_func -------------------------------------------------------


hmm_g_ep_probs_func <-
  brms::stanvar(
    scode = '
    vector hmm_g_ep_probs(vector block_grp, vector choice, vector reward, vector gamma, vector ep) {


    int nT = size(choice);

    vector[nT] Ps_out;

    vector[2] Ps;  // prob of the states, 1 - left, 2 - right
    real P_O_S1;   // p(O|S1) - O = {A,R} given left
    real P_O_S2;   // p(O|S2) - O = {A,R} given right
    real choice_ep; // p(A|R)

    vector[2] Ps_init; // initial prob/belief of the two states
    Ps_init = reptor(0.5, 2);

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

      // renew of emission prob: p(O|S1) p(O|S2); O is a pair between A(ction) and R(eward)
      // --> the probability of actually observing this outcome
      choice_ep = reward[n] >= 0 ? ep[n] : (1-ep[n]);
      P_O_S1 = 0.5 * ( (choice[n] == 1) ? choice_ep : (1-choice_ep) );
      P_O_S2 = 0.5 * ( (choice[n] == 2) ? choice_ep : (1-choice_ep) );

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


### hmm_rp_blm_probs_func ------------------------------------------------------


hmm_rp_blm_probs_func <-
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

### ql_a_it_blm_probs_func -----------------------------------------------------


ql_a_it_blm_probs_func <-
  brms::stanvar(
    scode = '
  vector ql_a_it_probs(vector choice, vector reward, vector alpha, vector tau) {
    int nT = size(choice);
    vector[nT] Ps_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real PE;            // prediction error
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

    Qs_init = rep_vector(0.0, 2);
    Qs = Qs_init;

    for (n in 1:nT)  {

      // get choice of current trial (either 1 or 2)
      ci = (choice[n]==1 ? 2 : 1);

      // compute action probabilities
      Ps = softmax(Qs*tau[n]);
      Ps_out[n] = Ps[2]; // predicting upper bound

      // value updating (learning)
      Qs[ci] += alpha[n] * (reward[n] - Qs[ci]);

    } // trial loop

    return Ps_out;
  }
    ',
    block = 'functions'
  )



### ql_a_it_probs_func -----------------------------------------------------


ql_a_it_probs_func <-
  brms::stanvar(
    scode = '
  vector ql_a_it_probs(vector block_grp, vector choice, vector reward, vector alpha, vector tau) {
    int nT = size(choice);
    vector[nT] Ps_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real PE;            // prediction error
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

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
      Qs[ci] += alpha[n] * PE;

    } // trial loop

    return Ps_out;
  }
    ',
    block = 'functions'
  )


### ql_a_2it_probs_func -----------------------------------------------------


ql_a_2it_probs_func <-
  brms::stanvar(
    scode = '
  vector ql_a_2it_probs(vector block_grp, vector choice, vector reward, vector alpha, vector taupos, vector tauneg) {
    int nT = size(choice);
    vector[nT] Ps_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real prev_reward;   // reward in previous_trial
    real tau;           // effective tau based on previous outcome
    real PE;            // prediction error
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

    Qs_init = rep_vector(0.0, 2);
    Qs = Qs_init;
    prev_reward = 0.0;

    for (n in 1:nT)  {

      if (n == 1 || block_grp[n]!=block_grp[n-1]) {
        Qs = Qs_init;
      }

      // get choice of current trial (either 1 or 2)
      ci = (choice[n]==1 ? 2 : 1);

      // compute action probabilities
      real tau = prev_reward >= 0 ? taupos[n] : tauneg[n];
      Ps = softmax(Qs*tau);
      Ps_out[n] = Ps[2]; // predicting upper bound

      // prediction error
      PE = (reward[n]) - Qs[ci];

      // value updating (learning)
      Qs[ci] += alpha[n] * PE;

      // save for next trial
      prev_reward = reward[n]

    } // trial loop

    return Ps_out;
  }
    ',
    block = 'functions'
  )


### ql_2a_it_probs_func -----------------------------------------------------


ql_2a_it_probs_func <-
  brms::stanvar(
    scode = '
  vector ql_2a_it_probs(vector block_grp, vector choice, vector reward, vector alphapos, vector alphaneg, vector tau) {
    int nT = size(choice);
    vector[nT] Ps_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real PE;            // prediction error
    real alpha;         // effective learning rate (alphapos or alphaneg, respectively)
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

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


### ql_2a_2it_probs_func -----------------------------------------------------


ql_2a_2it_probs_func <-
  brms::stanvar(
    scode = '
  vector ql_2a_2it_probs(vector block_grp, vector choice, vector reward, vector alphapos, vector alphaneg, vector taupos, vector tauneg) {
    int nT = size(choice);
    vector[nT] Ps_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real prev_reward;   // reward in previous trial
    real tau;           // effective tau beased on reward in previous trial
    real PE;            // prediction error
    real alpha;         // effective learning rate (alphapos or alphaneg, respectively)
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

    Qs_init = rep_vector(0.0, 2);
    Qs = Qs_init;
    prev_reward = 0.0;

    for (n in 1:nT)  {

      if (n == 1 || block_grp[n]!=block_grp[n-1]) {
        Qs = Qs_init;
      }

      // get choice of current trial (either 1 or 2)
      ci = (choice[n]==1 ? 2 : 1);

      // compute action probabilities
      tau = prev_reward >= 0 ? taupos[n] : tauneg[n];
      Ps = softmax(Qs*tau);
      Ps_out[n] = Ps[2]; // predicting upper bound

      // prediction error
      PE = (reward[n]) - Qs[ci];

      // value updating (learning)
      alpha = (PE >= 0) ? alphapos[n] : alphaneg[n];
      Qs[ci] += alpha * PE;

      // save for next trial
      prev_reward = reward[n];

    } // trial loop

    return Ps_out;
  }
    ',
    block = 'functions'
  )


### ql_a_xi_probs_func -----------------------------------------------------

ql_a_xi_probs_func <-
  brms::stanvar(
    scode = '
  vector ql_a_xi_probs(vector block_grp, vector choice, vector reward, vector alpha, vector xi) {
    int nT = size(choice);
    vector[nT] Ps_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real PE;            // prediction error
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

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
      Qs[ci] += alpha[n] * PE;

    } // trial loop

    return Ps_out;
  }
    ',
    block = 'functions'
  )

### ql_a_2xi_probs_func -----------------------------------------------------

ql_a_2xi_probs_func <-
  brms::stanvar(
    scode = '
  vector ql_a_2xi_probs(vector block_grp, vector choice, vector reward, vector alpha, vector xipos, vector xineg) {
    int nT = size(choice);
    vector[nT] Ps_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real prev_reward    // reward in previous trial
    real xi             // effective xi based on reward in previous trial
    real PE;            // prediction error
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

    Qs_init = rep_vector(0.0, 2);
    Qs = Qs_init;
    prev_reward = 0.0;

    for (n in 1:nT) {

      if (n == 1 || block_grp[n]!=block_grp[n-1]) {
        Qs = Qs_init;
      }

      // get choice of current trial (either 1 or 2)
      ci = (choice[n]==1 ? 2 : 1);

      // compute action probabilities
      xi = prev_reward >= 0 ? xipos[n] : xineg[n];
      Ps = softmax(Qs);
      Ps_out[n] = Ps[2] * (1-xi) + xi/2; // predicting upper bound

      // prediction error
      PE = (reward[n]) - Qs[ci];

      // value updating (learning)
      Qs[ci] += alpha[n] * PE;

      // save for next trial
      prev_reward = reward[n];

    } // trial loop

    return Ps_out;
  }
    ',
    block = 'functions'
  )

### ql_2a_xi_probs_func -----------------------------------------------------

ql_2a_xi_probs_func <-
  brms::stanvar(
    scode = '
  vector ql_2a_xi_probs(vector block_grp, vector choice, vector reward, vector alphapos, vector alphaneg, vector xi) {
    int nT = size(choice);
    vector[nT] Ps_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real PE;            // prediction error
    real alpha;         // effective learning rate (alphapos or alphaneg, respectively)
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

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

### ql_2a_2xi_probs_func -----------------------------------------------------

ql_2a_2xi_probs_func <-
  brms::stanvar(
    scode = '
  vector ql_2a_2xi_probs(vector block_grp, vector choice, vector reward, vector alphapos, vector alphaneg, vector xipos, vector xineg) {
    int nT = size(choice);
    vector[nT] Ps_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real prev_reward;   // reward in previous trial
    real xi;
    real PE;            // prediction error
    real alpha;         // effective learning rate (alphapos or alphaneg, respectively)
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

    Qs_init = rep_vector(0.0, 2);
    Qs = Qs_init;
    prev_reward = 0.0;

    for (n in 1:nT) {

      if (n == 1 || block_grp[n]!=block_grp[n-1]) {
        Qs = Qs_init;
      }

      // get choice of current trial (either 1 or 2)
      ci = (choice[n]==1 ? 2 : 1);

      // compute action probabilities
      xi = prev_reward >= 0 ? xipos[n] : xineg[n];
      Ps = softmax(Qs);
      Ps_out[n] = Ps[2] * (1-xi) + xi/2; // predicting upper bound

      // prediction error
      PE = (reward[n]) - Qs[ci];

      // value updating (learning)
      alpha = (PE >= 0) ? alphapos[n] : alphaneg[n];
      Qs[ci] += alpha * PE;

      // save for next trial
      prev_reward = reward[n];

    } // trial loop

    return Ps_out;
  }
    ',
    block = 'functions'
  )

### ql_a_rho_probs_func -----------------------------------------------------

ql_a_rho_probs_func <-
  brms::stanvar(
    scode = '
  vector ql_a_rho_probs(vector block_grp, vector choice, vector reward, vector alpha, vector rho) {
    int nT = size(choice);
    vector[nT] Ps_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real PE;            // prediction error
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

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
      Qs[ci] += alpha[n] * PE;

    } // trial loop

    return Ps_out;
  }
    ',
    block = 'functions'
  )

### ql_2a_rho_probs_func -----------------------------------------------------

ql_2a_rho_probs_func <-
  brms::stanvar(
    scode = '
  vector ql_2a_rho_probs(vector block_grp, vector choice, vector reward, vector alphapos, vector alphaneg, vector rho) {
    int nT = size(choice);
    vector[nT] Ps_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real PE;            // prediction error
    real alpha;         // effective learning rate (alphapos or alphaneg, respectively)
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

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

### ql_a_2rho_probs_func -----------------------------------------------------

ql_a_2rho_probs_func <-
  brms::stanvar(
    scode = '
  vector ql_a_2rho_probs(vector block_grp, vector choice, vector reward, vector alpha, vector rhopos, vector rhoneg) {
    int nT = size(choice);
    vector[nT] Ps_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real PE;            // prediction error
    real rho;           // effective reward sensitivity (alphapos or alphaneg, respectively)
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

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
      Qs[ci] += alpha[n] * PE;

    } // trial loop

    return Ps_out;
  }
    ',
    block = 'functions'
  )


### ql_2a_2rho_probs_func -----------------------------------------------------

ql_2a_2rho_probs_func <-
  brms::stanvar(
    scode = '
  vector ql_2a_2rho_probs(vector block_grp, vector choice, vector reward, vector alphapos, vector alphaneg, vector rhopos, vector rhoneg) {
    int nT = size(choice);
    vector[nT] Ps_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real PE;            // prediction error
    real alpha;         // effective learning rate (alphapos or alphaneg, respectively)
    real rho;           // effective reward sensitivity (rhopos or rhoneg, respectively)
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

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


### ql_a_it_fu_probs_func -----------------------------------------------------


ql_a_it_fu_probs_func <-
  brms::stanvar(
    scode = '
  vector ql_a_it_fu_probs(vector block_grp, vector choice, vector reward, vector alpha, vector tau) {
    int nT = size(choice);
    vector[nT] Ps_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real PE;            // prediction error
    real PE_uc;         // prediction error for unchosen option
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

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
      PE_uc = (reward[n]*-1) - Qs[3-ci];

      // value updating (learning)
      Qs[ci] += alpha[n] * PE;
      Qs[3-ci] += alpha[n] * PE_uc;

    } // trial loop

    return Ps_out;
  }
    ',
    block = 'functions'
  )


### ql_a_2it_fu_probs_func -----------------------------------------------------


ql_a_2it_fu_probs_func <-
  brms::stanvar(
    scode = '
  vector ql_a_2it_fu_probs(vector block_grp, vector choice, vector reward, vector alpha, vector taupos, vector tauneg) {
    int nT = size(choice);
    vector[nT] Ps_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real prev_reward;   // reward in previous_trial
    real tau;           // effective tau based on previous outcome
    real PE;            // prediction error
    real PE_uc;         // prediction error for unchosen option
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

    Qs_init = rep_vector(0.0, 2);
    Qs = Qs_init;
    prev_reward = 0.0;

    for (n in 1:nT)  {

      if (n == 1 || block_grp[n]!=block_grp[n-1]) {
        Qs = Qs_init;
      }

      // get choice of current trial (either 1 or 2)
      ci = (choice[n]==1 ? 2 : 1);

      // compute action probabilities
      real tau = prev_reward >= 0 ? taupos[n] : tauneg[n];
      Ps = softmax(Qs*tau);
      Ps_out[n] = Ps[2]; // predicting upper bound

      // prediction error
      PE = (reward[n]) - Qs[ci];
      PE_uc = (reward[n]*-1) - Qs[3-ci];

      // value updating (learning)
      Qs[ci] += alpha[n] * PE;
      Qs[3-ci] += alpha[n] * PE_uc;

      // save for next trial
      prev_reward = reward[n]

    } // trial loop

    return Ps_out;
  }
    ',
    block = 'functions'
  )


### ql_2a_it_fu_probs_func -----------------------------------------------------


ql_2a_it_fu_probs_func <-
  brms::stanvar(
    scode = '
  vector ql_2a_it_fu_probs(vector block_grp, vector choice, vector reward, vector alphapos, vector alphaneg, vector tau) {
    int nT = size(choice);
    vector[nT] Ps_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real PE;            // prediction error
    real PE_uc;         // prediction error for unchosen option
    real alpha;         // effective learning rate (alphapos or alphaneg, respectively)
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

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
      PE_uc = (reward[n]*-1) - Qs[3-ci];

      // value updating (learning)
      alpha = (PE >= 0) ? alphapos[n] : alphaneg[n];
      Qs[ci] += alpha * PE;
      Qs[3-ci] += alpha * PE_uc;

    } // trial loop

    return Ps_out;
  }
    ',
    block = 'functions'
  )


### ql_2a_2it_fu_probs_func -----------------------------------------------------


ql_2a_2it_fu_probs_func <-
  brms::stanvar(
    scode = '
  vector ql_2a_2it_fu_probs(vector block_grp, vector choice, vector reward, vector alphapos, vector alphaneg, vector taupos, vector tauneg) {
    int nT = size(choice);
    vector[nT] Ps_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real prev_reward;   // reward in previous trial
    real tau;           // effective tau beased on reward in previous trial
    real PE;            // prediction error
    real PE_uc;         // prediction error for unchosen option
    real alpha;         // effective learning rate (alphapos or alphaneg, respectively)
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

    Qs_init = rep_vector(0.0, 2);
    Qs = Qs_init;
    prev_reward = 0.0;

    for (n in 1:nT)  {

      if (n == 1 || block_grp[n]!=block_grp[n-1]) {
        Qs = Qs_init;
      }

      // get choice of current trial (either 1 or 2)
      ci = (choice[n]==1 ? 2 : 1);

      // compute action probabilities
      tau = prev_reward >= 0 ? taupos[n] : tauneg[n];
      Ps = softmax(Qs*tau);
      Ps_out[n] = Ps[2]; // predicting upper bound

      // prediction error
      PE = (reward[n]) - Qs[ci];
      PE_uc = (reward[n]*-1) - Qs[3-ci];

      // value updating (learning)
      alpha = (PE >= 0) ? alphapos[n] : alphaneg[n];
      Qs[ci] += alpha * PE;
      Qs[3-ci] += alpha * PE_uc;

      // save for next trial
      prev_reward = reward[n];

    } // trial loop

    return Ps_out;
  }
    ',
    block = 'functions'
  )


### ql_a_xi_fu_probs_func -----------------------------------------------------

ql_a_xi_fu_probs_func <-
  brms::stanvar(
    scode = '
  vector ql_a_xi_fu_probs(vector block_grp, vector choice, vector reward, vector alpha, vector xi) {
    int nT = size(choice);
    vector[nT] Ps_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real PE;            // prediction error
    real PE_uc;         // prediction error for unchosen option
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

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
      PE_uc = (reward[n]*-1) - Qs[3-ci];

      // value updating (learning)
      Qs[ci] += alpha[n] * PE;
      Qs[3-ci] += alpha[n] * PE_uc;

    } // trial loop

    return Ps_out;
  }
    ',
    block = 'functions'
  )

### ql_a_2xi_fu_probs_func -----------------------------------------------------

ql_a_2xi_fu_probs_func <-
  brms::stanvar(
    scode = '
  vector ql_a_2xi_fu_probs(vector block_grp, vector choice, vector reward, vector alpha, vector xipos, vector xineg) {
    int nT = size(choice);
    vector[nT] Ps_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real prev_reward    // reward in previous trial
    real xi             // effective xi based on reward in previous trial
    real PE;            // prediction error
    real PE_uc;         // prediction error for unchosen option
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

    Qs_init = rep_vector(0.0, 2);
    Qs = Qs_init;
    prev_reward = 0.0;

    for (n in 1:nT) {

      if (n == 1 || block_grp[n]!=block_grp[n-1]) {
        Qs = Qs_init;
      }

      // get choice of current trial (either 1 or 2)
      ci = (choice[n]==1 ? 2 : 1);

      // compute action probabilities
      xi = prev_reward >= 0 ? xipos[n] : xineg[n];
      Ps = softmax(Qs);
      Ps_out[n] = Ps[2] * (1-xi) + xi/2; // predicting upper bound

      // prediction error
      PE = (reward[n]) - Qs[ci];
      PE_uc = (reward[n]*-1) - Qs[3-ci];

      // value updating (learning)
      Qs[ci] += alpha[n] * PE;
      Qs[3-ci] += alpha[n] * PE_uc;

      // save for next trial
      prev_reward = reward[n];

    } // trial loop

    return Ps_out;
  }
    ',
    block = 'functions'
  )

### ql_2a_xi_fu_probs_func -----------------------------------------------------

ql_2a_xi_fu_probs_func <-
  brms::stanvar(
    scode = '
  vector ql_2a_xi_fu_probs(vector block_grp, vector choice, vector reward, vector alphapos, vector alphaneg, vector xi) {
    int nT = size(choice);
    vector[nT] Ps_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real PE;            // prediction error
    real PE_uc;         // prediction error for unchosen option
    real alpha;         // effective learning rate (alphapos or alphaneg, respectively)
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

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
      PE_uc = (reward[n]*-1) - Qs[3-ci];

      // value updating (learning)
      alpha = (PE >= 0) ? alphapos[n] : alphaneg[n];
      Qs[ci] += alpha * PE;
      Qs[3-ci] += alpha * PE_uc;

    } // trial loop

    return Ps_out;
  }
    ',
    block = 'functions'
  )

### ql_2a_2xi_fu_probs_func -----------------------------------------------------

ql_2a_2xi_fu_probs_func <-
  brms::stanvar(
    scode = '
  vector ql_2a_2xi_fu_probs(vector block_grp, vector choice, vector reward, vector alphapos, vector alphaneg, vector xipos, vector xineg) {
    int nT = size(choice);
    vector[nT] Ps_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real prev_reward;   // reward in previous trial
    real xi;
    real PE;            // prediction error
    real PE_uc;         // prediction error for unchosen option
    real alpha;         // effective learning rate (alphapos or alphaneg, respectively)
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

    Qs_init = rep_vector(0.0, 2);
    Qs = Qs_init;
    prev_reward = 0.0;

    for (n in 1:nT) {

      if (n == 1 || block_grp[n]!=block_grp[n-1]) {
        Qs = Qs_init;
      }

      // get choice of current trial (either 1 or 2)
      ci = (choice[n]==1 ? 2 : 1);

      // compute action probabilities
      xi = prev_reward >= 0 ? xipos[n] : xineg[n];
      Ps = softmax(Qs);
      Ps_out[n] = Ps[2] * (1-xi) + xi/2; // predicting upper bound

      // prediction error
      PE = (reward[n]) - Qs[ci];
      PE_uc = (reward[n]*-1) - Qs[3-ci];

      // value updating (learning)
      alpha = (PE >= 0) ? alphapos[n] : alphaneg[n];
      Qs[ci] += alpha * PE;
      Qs[3-ci] += alpha[n] * PE_uc;

      // save for next trial
      prev_reward = reward[n];

    } // trial loop

    return Ps_out;
  }
    ',
    block = 'functions'
  )

### ql_a_rho_fu_probs_func -----------------------------------------------------

ql_a_rho_fu_probs_func <-
  brms::stanvar(
    scode = '
  vector ql_a_rho_fu_probs(vector block_grp, vector choice, vector reward, vector alpha, vector rho) {
    int nT = size(choice);
    vector[nT] Ps_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real PE;            // prediction error
    real PE_uc;         // prediction error for unchosen option
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

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
      PE_uc = (rho[n]*reward[n]*-1) - Qs[3-ci];

      // value updating (learning)
      Qs[ci] += alpha[n] * PE;
      Qs[3-ci] += alpha[n] * PE_uc;

    } // trial loop

    return Ps_out;
  }
    ',
    block = 'functions'
  )

### ql_a_2rho_fu_probs_func -----------------------------------------------------

ql_a_2rho_fu_probs_func <-
  brms::stanvar(
    scode = '
  vector ql_a_2rho_fu_probs(vector block_grp, vector choice, vector reward, vector alpha, vector rhopos, vector rhoneg) {

    int nT = size(choice);
    vector[nT] Ps_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real PE;            // prediction error
    real PE_uc;         // prediction error for uchosen options
    real rho;           // effective reward sensitivity (alphapos or alphaneg, respectively)
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

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
      PE_uc = (rho*(reward[n]*-1)) - Qs[3-ci];


      // value updating (learning)
      Qs[ci] += alpha[n] * PE;
      Qs[3-ci] += alpha[n] * PE_uc; # ficticious update

    } // trial loop

    return Ps_out;
  }
    ',
    block = 'functions'
  )

### ql_2a_rho_fu_probs_func -----------------------------------------------------

ql_2a_rho_fu_probs_func <-
  brms::stanvar(
    scode = '
  vector ql_a_rho_fu_probs(vector block_grp, vector choice, vector reward, vector alphapos, vector alphaneg, vector rho) {
    int nT = size(choice);
    vector[nT] Ps_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real PE;            // prediction error
    real PE_uc;         // prediction error for unchosen option
    real alpha;         // effective learning
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

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
      PE_uc = (rho[n]*reward[n]*-1) - Qs[3-ci];

      // value updating (learning)
      alpha = (PE >= 0) ? alphapos[n] : alphaneg[n];
      Qs[ci] += alpha * PE;
      Qs[3-ci] += alpha * PE_uc;

    } // trial loop

    return Ps_out;
  }
    ',
    block = 'functions'
  )

### ql_2a_2rho_fu_probs_func -----------------------------------------------------

ql_2a_2rho_fu_probs_func <-
  brms::stanvar(
    scode = '
  vector ql_a_rho_fu_probs(vector block_grp, vector choice, vector reward, vector alphapos, vector alphaneg, vector rhopoe, vector rhoneg) {
    int nT = size(choice);
    vector[nT] Ps_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real PE;            // prediction error
    real PE_uc;         // prediction error for unchosen option
    real alpha;         // effective learning
    real rho;           // effective reward sensitivity
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

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
      PE_uc = (rho*reward[n]*-1) - Qs[3-ci];

      // value updating (learning)
      alpha = (PE >= 0) ? alphapos[n] : alphaneg[n];
      Qs[ci] += alpha * PE;
      Qs[3-ci] += alpha * PE_uc;

    } // trial loop

    return Ps_out;
  }
    ',
    block = 'functions'
  )


### ql_a_it_kfu_probs_func -----------------------------------------------------


ql_a_it_kfu_probs_func <-
  brms::stanvar(
    scode = '
  vector ql_a_it_kfu_probs(vector block_grp, vector choice, vector reward, vector alpha, vector tau, vector kappa) {
    int nT = size(choice);
    vector[nT] Ps_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real PE;            // prediction error
    real PE_uc;         // prediction error for unchosen option
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

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
      PE_uc = (reward[n]*-1) - Qs[3-ci];

      // value updating (learning)
      Qs[ci] += alpha[n] * PE;
      Qs[3-ci] += kappa[n] * alpha[n] * PE_uc;

    } // trial loop

    return Ps_out;
  }
    ',
    block = 'functions'
  )


### ql_a_2it_kfu_probs_func -----------------------------------------------------


ql_a_2it_kfu_probs_func <-
  brms::stanvar(
    scode = '
  vector ql_a_2it_kfu_probs(vector block_grp, vector choice, vector reward, vector alpha, vector taupos, vector tauneg, vector kappa) {
    int nT = size(choice);
    vector[nT] Ps_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real prev_reward;   // reward in previous_trial
    real tau;           // effective tau based on previous outcome
    real PE;            // prediction error
    real PE_uc;         // prediction error for unchosen option
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

    Qs_init = rep_vector(0.0, 2);
    Qs = Qs_init;
    prev_reward = 0.0;

    for (n in 1:nT)  {

      if (n == 1 || block_grp[n]!=block_grp[n-1]) {
        Qs = Qs_init;
      }

      // get choice of current trial (either 1 or 2)
      ci = (choice[n]==1 ? 2 : 1);

      // compute action probabilities
      real tau = prev_reward >= 0 ? taupos[n] : tauneg[n];
      Ps = softmax(Qs*tau);
      Ps_out[n] = Ps[2]; // predicting upper bound

      // prediction error
      PE = (reward[n]) - Qs[ci];
      PE_uc = (reward[n]*-1) - Qs[3-ci];

      // value updating (learning)
      Qs[ci] += alpha[n] * PE;
      Qs[3-ci] += kappa[n] * alpha[n] * PE_uc;

      // save for next trial
      prev_reward = reward[n]

    } // trial loop

    return Ps_out;
  }
    ',
    block = 'functions'
  )


### ql_2a_it_kfu_probs_func -----------------------------------------------------


ql_2a_it_kfu_probs_func <-
  brms::stanvar(
    scode = '
  vector ql_2a_it_kfu_probs(vector block_grp, vector choice, vector reward, vector alphapos, vector alphaneg, vector tau, vector kappa) {
    int nT = size(choice);
    vector[nT] Ps_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real PE;            // prediction error
    real PE_uc;         // prediction error for unchosen option
    real alpha;         // effective learning rate (alphapos or alphaneg, respectively)
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

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
      PE_uc = (reward[n]*-1) - Qs[3-ci];

      // value updating (learning)
      alpha = (PE >= 0) ? alphapos[n] : alphaneg[n];
      Qs[ci] += alpha * PE;
      Qs[3-ci] += kappa[n] * alpha * PE_uc;

    } // trial loop

    return Ps_out;
  }
    ',
    block = 'functions'
  )


### ql_2a_2it_kfu_probs_func -----------------------------------------------------


ql_2a_2it_kfu_probs_func <-
  brms::stanvar(
    scode = '
  vector ql_2a_2it_kfu_probs(vector block_grp, vector choice, vector reward, vector alphapos, vector alphaneg, vector taupos, vector tauneg, vector kappa) {
    int nT = size(choice);
    vector[nT] Ps_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real prev_reward;   // reward in previous trial
    real tau;           // effective tau beased on reward in previous trial
    real PE;            // prediction error
    real PE_uc;         // prediction error for unchosen option
    real alpha;         // effective learning rate (alphapos or alphaneg, respectively)
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

    Qs_init = rep_vector(0.0, 2);
    Qs = Qs_init;
    prev_reward = 0.0;

    for (n in 1:nT)  {

      if (n == 1 || block_grp[n]!=block_grp[n-1]) {
        Qs = Qs_init;
      }

      // get choice of current trial (either 1 or 2)
      ci = (choice[n]==1 ? 2 : 1);

      // compute action probabilities
      tau = prev_reward >= 0 ? taupos[n] : tauneg[n];
      Ps = softmax(Qs*tau);
      Ps_out[n] = Ps[2]; // predicting upper bound

      // prediction error
      PE = (reward[n]) - Qs[ci];
      PE_uc = (reward[n]*-1) - Qs[3-ci];

      // value updating (learning)
      alpha = (PE >= 0) ? alphapos[n] : alphaneg[n];
      Qs[ci] += alpha * PE;
      Qs[3-ci] += kappa[n] * alpha * PE_uc;

      // save for next trial
      prev_reward = reward[n];

    } // trial loop

    return Ps_out;
  }
    ',
    block = 'functions'
  )


### ql_a_xi_kfu_probs_func -----------------------------------------------------

ql_a_xi_kfu_probs_func <-
  brms::stanvar(
    scode = '
  vector ql_a_xi_kfu_probs(vector block_grp, vector choice, vector reward, vector alpha, vector xi, vector kappa) {
    int nT = size(choice);
    vector[nT] Ps_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real PE;            // prediction error
    real PE_uc;         // prediction error for unchosen option
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

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
      PE_uc = (reward[n]*-1) - Qs[3-ci];

      // value updating (learning)
      Qs[ci] += alpha[n] * PE;
      Qs[3-ci] += kappa[n] * alpha[n] * PE_uc;

    } // trial loop

    return Ps_out;
  }
    ',
    block = 'functions'
  )

### ql_a_2xi_kfu_probs_func -----------------------------------------------------

ql_a_2xi_kfu_probs_func <-
  brms::stanvar(
    scode = '
  vector ql_a_2xi_kfu_probs(vector block_grp, vector choice, vector reward, vector alpha, vector xipos, vector xineg, vector kappa) {
    int nT = size(choice);
    vector[nT] Ps_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real prev_reward    // reward in previous trial
    real xi             // effective xi based on reward in previous trial
    real PE;            // prediction error
    real PE_uc;         // prediction error for unchosen option
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

    Qs_init = rep_vector(0.0, 2);
    Qs = Qs_init;
    prev_reward = 0.0;

    for (n in 1:nT) {

      if (n == 1 || block_grp[n]!=block_grp[n-1]) {
        Qs = Qs_init;
      }

      // get choice of current trial (either 1 or 2)
      ci = (choice[n]==1 ? 2 : 1);

      // compute action probabilities
      xi = prev_reward >= 0 ? xipos[n] : xineg[n];
      Ps = softmax(Qs);
      Ps_out[n] = Ps[2] * (1-xi) + xi/2; // predicting upper bound

      // prediction error
      PE = (reward[n]) - Qs[ci];
      PE_uc = (reward[n]*-1) - Qs[3-ci];

      // value updating (learning)
      Qs[ci] += alpha[n] * PE;
      Qs[3-ci] += kappa[n] * alpha[n] * PE_uc;

      // save for next trial
      prev_reward = reward[n];

    } // trial loop

    return Ps_out;
  }
    ',
    block = 'functions'
  )

### ql_2a_xi_kfu_probs_func -----------------------------------------------------

ql_2a_xi_kfu_probs_func <-
  brms::stanvar(
    scode = '
  vector ql_2a_xi_kfu_probs(vector block_grp, vector choice, vector reward, vector alphapos, vector alphaneg, vector xi, vector kappa) {
    int nT = size(choice);
    vector[nT] Ps_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real PE;            // prediction error
    real PE_uc;         // prediction error for unchosen option
    real alpha;         // effective learning rate (alphapos or alphaneg, respectively)
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

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
      PE_uc = (reward[n]*-1) - Qs[3-ci];

      // value updating (learning)
      alpha = (PE >= 0) ? alphapos[n] : alphaneg[n];
      Qs[ci] += alpha * PE;
      Qs[3-ci] += kappa[n] * alpha * PE_uc;

    } // trial loop

    return Ps_out;
  }
    ',
    block = 'functions'
  )

### ql_2a_2xi_kfu_probs_func -----------------------------------------------------

ql_2a_2xi_kfu_probs_func <-
  brms::stanvar(
    scode = '
  vector ql_2a_2xi_kfu_probs(vector block_grp, vector choice, vector reward, vector alphapos, vector alphaneg, vector xipos, vector xineg, vector kappa) {
    int nT = size(choice);
    vector[nT] Ps_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real prev_reward;   // reward in previous trial
    real xi;
    real PE;            // prediction error
    real PE_uc;         // prediction error for unchosen option
    real alpha;         // effective learning rate (alphapos or alphaneg, respectively)
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

    Qs_init = rep_vector(0.0, 2);
    Qs = Qs_init;
    prev_reward = 0.0;

    for (n in 1:nT) {

      if (n == 1 || block_grp[n]!=block_grp[n-1]) {
        Qs = Qs_init;
      }

      // get choice of current trial (either 1 or 2)
      ci = (choice[n]==1 ? 2 : 1);

      // compute action probabilities
      xi = prev_reward >= 0 ? xipos[n] : xineg[n];
      Ps = softmax(Qs);
      Ps_out[n] = Ps[2] * (1-xi) + xi/2; // predicting upper bound

      // prediction error
      PE = (reward[n]) - Qs[ci];
      PE_uc = (reward[n]*-1) - Qs[3-ci];

      // value updating (learning)
      alpha = (PE >= 0) ? alphapos[n] : alphaneg[n];
      Qs[ci] += alpha * PE;
      Qs[3-ci] += kappa[n] * alpha[n] * PE_uc;

      // save for next trial
      prev_reward = reward[n];

    } // trial loop

    return Ps_out;
  }
    ',
    block = 'functions'
  )

### ql_a_rho_kfu_probs_func -----------------------------------------------------

ql_a_rho_kfu_probs_func <-
  brms::stanvar(
    scode = '
  vector ql_a_rho_kfu_probs(vector block_grp, vector choice, vector reward, vector alpha, vector rho, vector kappa) {
    int nT = size(choice);
    vector[nT] Ps_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real PE;            // prediction error
    real PE_uc;         // prediction error for unchosen option
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

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
      PE_uc = (rho[n]*reward[n]*-1) - Qs[3-ci];

      // value updating (learning)
      Qs[ci] += alpha[n] * PE;
      Qs[3-ci] += kappa[n] * alpha[n] * PE_uc;

    } // trial loop

    return Ps_out;
  }
    ',
    block = 'functions'
  )

### ql_a_2rho_kfu_probs_func -----------------------------------------------------

ql_a_2rho_kfu_probs_func <-
  brms::stanvar(
    scode = '
  vector ql_a_2rho_kfu_probs(vector block_grp, vector choice, vector reward, vector alpha, vector rhopos, vector rhoneg, vector kappa) {

    int nT = size(choice);
    vector[nT] Ps_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real PE;            // prediction error
    real PE_uc;         // prediction error for uchosen options
    real rho;           // effective reward sensitivity (alphapos or alphaneg, respectively)
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

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
      PE_uc = (rho*(reward[n]*-1)) - Qs[3-ci];


      // value updating (learning)
      Qs[ci] += alpha[n] * PE;
      Qs[3-ci] += kappa[n] * alpha[n] * PE_uc; # ficticious update

    } // trial loop

    return Ps_out;
  }
    ',
    block = 'functions'
  )




### ql_2a_rho_kfu_probs_func -----------------------------------------------------

ql_2a_rho_kfu_probs_func <-
  brms::stanvar(
    scode = '
  vector ql_a_rho_kfu_probs(vector block_grp, vector choice, vector reward, vector alphapos, vector alphaneg, vector rho, vetor kappa) {
    int nT = size(choice);
    vector[nT] Ps_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real PE;            // prediction error
    real PE_uc;         // prediction error for unchosen option
    real alpha;         // effective learning
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

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
      PE_uc = (rho[n]*reward[n]*-1) - Qs[3-ci];

      // value updating (learning)
      alpha = (PE >= 0) ? alphapos[n] : alphaneg[n];
      Qs[ci] += alpha * PE;
      Qs[3-ci] += kappa [n] * alpha * PE_uc;

    } // trial loop

    return Ps_out;
  }
    ',
    block = 'functions'
  )

### ql_2a_2rho_kfu_probs_func -----------------------------------------------------

ql_2a_2rho_kfu_probs_func <-
  brms::stanvar(
    scode = '
  vector ql_a_rho_kfu_probs(vector block_grp, vector choice, vector reward, vector alphapos, vector alphaneg, vector rhopoe, vector rhoneg) {
    int nT = size(choice);
    vector[nT] Ps_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real PE;            // prediction error
    real PE_uc;         // prediction error for unchosen option
    real alpha;         // effective learning
    real rho;           // effective reward sensitivity
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

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
      PE_uc = (rho*reward[n]*-1) - Qs[3-ci];

      // value updating (learning)
      alpha = (PE >= 0) ? alphapos[n] : alphaneg[n];
      Qs[ci] += alpha * PE;
      Qs[3-ci] += kappa [n] * alpha * PE_uc;

    } // trial loop

    return Ps_out;
  }
    ',
    block = 'functions'
  )
### ql_2a_2rho_fu_probs_func -----------------------------------------------------

ql_2a_2rho_fu_probs_func <-
  brms::stanvar(
    scode = '
  vector ql_2a_2rho_fu_probs(vector block_grp, vector choice, vector reward, vector alphapos, vector alphaneg, vector rhopos, vector rhoneg, vector kappa) {
    int nT = size(choice);
    vector[nT] Ps_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real PE;            // prediction error
    real PE_uc;         // prediction error for unchosen option
    real alpha;         // effective learning rate (alphapos or alphaneg, respectively)
    real rho;           // effective reward sensitivity (rhopos or rhoneg, respectively)
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

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
      PE_uc = (rho*(reward[n]*-1)) - Qs[3-ci];

      // value updating (learning)
      alpha = (PE >= 0) ? alphapos[n] : alphaneg[n];
      Qs[ci] += alpha * PE;
      Qs[3-ci] += kappa[n] * alpha * PE_uc;

    } // trial loop

    return Ps_out;
  }
    ',
    block = 'functions'
  )

### qlddm_2a_qdiff_func ---------------------------------------------


qlddm_2a_qdiff_func <-
  brms::stanvar(
    scode = '
  vector ql_2a_qdiff(vector block_grp, vector choice, vector reward, vector alphapos, vector alphaneg) {
    int nT = size(choice);
    vector[nT] Qdiffs_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real PE;            // prediction error
    real alpha;         // effective learning rate (alphapos or alphaneg, respectively)
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

    Qs_init = rep_vector(0.0, 2);
    Qs = Qs_init;

    for (n in 1:nT)  {

      if (n == 1 || block_grp[n]!=block_grp[n-1]) {
        Qs = Qs_init;
      }
      if(block_grp[n]==0) {
        Qdiffs_out[n] = 0.0;
        continue;
      }

      // get choice of current trial (either 1 or 2)
      ci = (choice[n]==1 ? 2 : 1);

      // compute action probabilities
      Qdiffs_out[n] = Qs[2]-Qs[1]; // predicting upper bound

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

### qlddm_a_qdiff_nu_func ---------------------------------------------


qlddm_a_qdiff_nu_func <-
  brms::stanvar(
    scode = '
  vector qlddm_a_qdiff_nu(vector block_grp, vector choice, vector reward, vector eta, vector nu) {
    int nT = size(choice);
    vector[nT] nu_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real PE;            // prediction error
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

    Qs_init = rep_vector(0.0, 2);
    Qs = Qs_init;

    for (n in 1:nT)  {

      if (n == 1 || block_grp[n]!=block_grp[n-1]) {
        Qs = Qs_init;
      }
      if(block_grp[n]==0) {
        nu_out[n] = 0.0;
        continue;
      }

      // get choice of current trial (either 1 or 2)
      ci = (choice[n]==1 ? 2 : 1);

      // compute action probabilities
      nu_out[n] = nu[n]*(Qs[2]-Qs[1]); // predicting upper bound

      // prediction error
      PE = (reward[n]) - Qs[ci];

      // value updating (learning)
      Qs[ci] += eta[n] * PE;

    } // trial loop

    return nu_out;
  }
    ',
    block = 'functions'
  )

### qlddm_a_rho_qdiff_nu_func ---------------------------------------------


qlddm_a_rho_qdiff_nu_func <-
  brms::stanvar(
    scode = '
  vector qlddm_a_rho_qdiff_nu(vector block_grp, vector choice, vector reward, vector eta, vector rho, vector nu) {
    int nT = size(choice);
    vector[nT] nu_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real PE;            // prediction error
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

    Qs_init = rep_vector(0.0, 2);
    Qs = Qs_init;

    for (n in 1:nT)  {

      if (n == 1 || block_grp[n]!=block_grp[n-1]) {
        Qs = Qs_init;
      }
      if(block_grp[n]==0) {
        nu_out[n] = 0.0;
        continue;
      }

      // get choice of current trial (either 1 or 2)
      ci = (choice[n]==1 ? 2 : 1);

      // compute action probabilities
      nu_out[n] = nu[n]*(Qs[2]-Qs[1]); // predicting upper bound

      // prediction error
      PE = (reward[n] * rho[n]) - Qs[ci];

      // value updating (learning)
      Qs[ci] += eta[n] * PE;

    } // trial loop

    return nu_out;
  }
    ',
    block = 'functions'
  )

### qlddm_a_2rho_qdiff_nu_func ---------------------------------------------


qlddm_a_rho_qdiff_nu_func <-
  brms::stanvar(
    scode = '
  vector qlddm_a_rho_qdiff_nu(vector block_grp, vector choice, vector reward, vector eta, vector rhopos, vector rhoneg, vector nu) {
    int nT = size(choice);
    vector[nT] nu_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real PE;            // prediction error
    real rho;           // effective reward sensitivity (rhopos pr rhoneg, respectively)
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

    Qs_init = rep_vector(0.0, 2);
    Qs = Qs_init;

    for (n in 1:nT)  {

      if (n == 1 || block_grp[n]!=block_grp[n-1]) {
        Qs = Qs_init;
      }
      if(block_grp[n]==0) {
        nu_out[n] = 0.0;
        continue;
      }

      // get choice of current trial (either 1 or 2)
      ci = (choice[n]==1 ? 2 : 1);

      // compute action probabilities
      nu_out[n] = nu[n]*(Qs[2]-Qs[1]); // predicting upper bound

      // prediction error
      rho = reward[n] >= 0 ? rhopos[n] : rhoneg[n];
      PE = (reward[n] * rho) - Qs[ci];

      // value updating (learning)
      Qs[ci] += eta[n] * PE;

    } // trial loop

    return nu_out;
  }
    ',
    block = 'functions'
  )

### qlddm_2a_qdiff_nu_func ---------------------------------------------


qlddm_2a_qdiff_nu_func <-
  brms::stanvar(
    scode = '
  vector qlddm_2a_qdiff_nu(vector block_grp, vector choice, vector reward, vector alphapos, vector alphaneg, vector nu) {
    int nT = size(choice);
    vector[nT] nu_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real PE;            // prediction error
    real alpha;         // effective learning rate (alphapos or alphaneg, respectively)
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

    Qs_init = rep_vector(0.0, 2);
    Qs = Qs_init;

    for (n in 1:nT)  {

      if (n == 1 || block_grp[n]!=block_grp[n-1]) {
        Qs = Qs_init;
      }
      if(block_grp[n]==0) {
        nu_out[n] = 0.0;
        continue;
      }

      // get choice of current trial (either 1 or 2)
      ci = (choice[n]==1 ? 2 : 1);

      // compute action probabilities
      nu_out[n] = nu[n]*(Qs[2]-Qs[1]); // predicting upper bound

      // prediction error
      PE = (reward[n]) - Qs[ci];

      // value updating (learning)
      alpha = (PE >= 0) ? alphapos[n] : alphaneg[n];
      Qs[ci] += alpha * PE;

    } // trial loop

    return nu_out;
  }
    ',
    block = 'functions'
  )

### qlddm_2a_rho_qdiff_nu_func ---------------------------------------------


qlddm_2a_rho_qdiff_nu_func <-
  brms::stanvar(
    scode = '
  vector qlddm_2a_rho_qdiff_nu(vector block_grp, vector choice, vector reward, vector alphapos, vector alphaneg, vector rho, vector nu) {
    int nT = size(choice);
    vector[nT] nu_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real PE;            // prediction error
    real alpha;         // effective learning rate (alphapos or alphaneg, respectively)
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

    Qs_init = rep_vector(0.0, 2);
    Qs = Qs_init;

    for (n in 1:nT)  {

      if (n == 1 || block_grp[n]!=block_grp[n-1]) {
        Qs = Qs_init;
      }
      if(block_grp[n]==0) {
        nu_out[n] = 0.0;
        continue;
      }

      // get choice of current trial (either 1 or 2)
      ci = (choice[n]==1 ? 2 : 1);

      // compute action probabilities
      nu_out[n] = nu[n]*(Qs[2]-Qs[1]); // predicting upper bound

      // prediction error
      PE = (reward[n] * rho[n]) - Qs[ci];

      // value updating (learning)
      alpha = (PE >= 0) ? alphapos[n] : alphaneg[n];
      Qs[ci] += alpha * PE;

    } // trial loop

    return nu_out;
  }
    ',
    block = 'functions'
  )

### qlddm_2a_2rho_qdiff_nu_func ---------------------------------------------


qlddm_2a_2rho_qdiff_nu_func <-
  brms::stanvar(
    scode = '
  vector qlddm_2a_2rho_qdiff_nu(vector block_grp, vector choice, vector reward, vector alphapos, vector alphaneg, vector rhopos, vector rhoneg, vector nu) {
    int nT = size(choice);
    vector[nT] nu_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real PE;            // prediction error
    real rho;           // effective reward sensitivity (rhopos or rhoneg, respectively)
    real alpha;         // effective learning rate (alphapos or alphaneg, respectively)
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

    Qs_init = rep_vector(0.0, 2);
    Qs = Qs_init;

    for (n in 1:nT)  {

      if (n == 1 || block_grp[n]!=block_grp[n-1]) {
        Qs = Qs_init;
      }
      if(block_grp[n]==0) {
        nu_out[n] = 0.0;
        continue;
      }

      // get choice of current trial (either 1 or 2)
      ci = (choice[n]==1 ? 2 : 1);

      // compute action probabilities
      nu_out[n] = nu[n]*(Qs[2]-Qs[1]); // predicting upper bound

      // prediction error
      rho = reward[n] >= 0 ? rhopos[n] : rhoneg[n];
      PE = (reward[n] * rho) - Qs[ci];

      // value updating (learning)
      alpha = (PE >= 0) ? alphapos[n] : alphaneg[n];
      Qs[ci] += alpha * PE;

    } // trial loop

    return nu_out;
  }
    ',
    block = 'functions'
  )

### qlddm_a_qdiff_num_func ---------------------------------------------


qlddm_a_qdiff_num_func <-
  brms::stanvar(
    scode = '
  vector qlddm_a_qdiff_nu(vector block_grp, vector choice, vector reward, vector eta, vector nu, vector numax) {
    int nT = size(choice);
    vector[nT] nu_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real PE;            // prediction error
    resal nut;          // slope of random walk in trial t
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

    Qs_init = rep_vector(0.0, 2);
    Qs = Qs_init;

    for (n in 1:nT)  {

      if (n == 1 || block_grp[n]!=block_grp[n-1]) {
        Qs = Qs_init;
      }
      if(block_grp[n]==0) {
        nu_out[n] = 0.0;
        continue;
      }

      // get choice of current trial (either 1 or 2)
      ci = (choice[n]==1 ? 2 : 1);

      // compute action probabilities
      nut = nu[n]*(Qs[2]-Qs[1]); // predicting upper bound
      nu_out[n] = 2 * numax[s] / (1 + exp(-nut)) - numax[s];

      // prediction error
      PE = (reward[n]) - Qs[ci];

      // value updating (learning)
      Qs[ci] += eta[n] * PE;

    } // trial loop

    return nu_out;
  }
    ',
    block = 'functions'
  )







### qlddm_a_rho_qdiff_num_func ---------------------------------------------


qlddm_a_rho_qdiff_num_func <-
  brms::stanvar(
    scode = '
  vector qlddm_a_rho_qdiff_nu(vector block_grp, vector choice, vector reward, vector eta, vector rho, vector nu, vector numax) {
    int nT = size(choice);
    vector[nT] nu_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real PE;            // prediction error
    resal nut;          // slope of random walk in trial t
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

    Qs_init = rep_vector(0.0, 2);
    Qs = Qs_init;

    for (n in 1:nT)  {

      if (n == 1 || block_grp[n]!=block_grp[n-1]) {
        Qs = Qs_init;
      }
      if(block_grp[n]==0) {
        nu_out[n] = 0.0;
        continue;
      }

      // get choice of current trial (either 1 or 2)
      ci = (choice[n]==1 ? 2 : 1);

      // compute action probabilities
      nut = nu[n]*(Qs[2]-Qs[1]); // predicting upper bound
      nu_out[n] = 2 * numax[s] / (1 + exp(-nut)) - numax[s];

      // prediction error
      PE = (reward[n] * rho[n]) - Qs[ci];

      // value updating (learning)
      Qs[ci] += eta[n] * PE;

    } // trial loop

    return nu_out;
  }
    ',
    block = 'functions'
  )







### qlddm_a_2rho_qdiff_num_func ---------------------------------------------


qlddm_a_2rho_qdiff_num_func <-
  brms::stanvar(
    scode = '
  vector qlddm_a_2rho_qdiff_nu(vector block_grp, vector choice, vector reward, vector eta, vector rhopos, vector rhoneg, vector nu, vector numax) {
    int nT = size(choice);
    vector[nT] nu_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real PE;            // prediction error
    real rho;           // effective reward sensitivity (rhopos or rhoneg, respectively)
    resal nut;          // slope of random walk in trial t
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

    Qs_init = rep_vector(0.0, 2);
    Qs = Qs_init;

    for (n in 1:nT)  {

      if (n == 1 || block_grp[n]!=block_grp[n-1]) {
        Qs = Qs_init;
      }
      if(block_grp[n]==0) {
        nu_out[n] = 0.0;
        continue;
      }

      // get choice of current trial (either 1 or 2)
      ci = (choice[n]==1 ? 2 : 1);

      // compute action probabilities
      nut = nu[n]*(Qs[2]-Qs[1]); // predicting upper bound
      nu_out[n] = 2 * numax[s] / (1 + exp(-nut)) - numax[s];

      // prediction error
      rho = reward[n] >= 0 ? rhopos[n] : rhoneg[n];
      PE = (reward[n] * rho) - Qs[ci];

      // value updating (learning)
      Qs[ci] += eta[n] * PE;

    } // trial loop

    return nu_out;
  }
    ',
    block = 'functions'
  )







### qlddm_2a_qdiff_num_func ---------------------------------------------


qlddm_2a_qdiff_num_func <-
  brms::stanvar(
    scode = '
  vector qlddm_2a_qdiff_nu(vector block_grp, vector choice, vector reward, vector alphapos, vector alphaneg, vector nu, vector numax) {
    int nT = size(choice);
    vector[nT] nu_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real PE;            // prediction error
    real nut;           // linear slope of random walk in trial t
    real alpha;         // effective learning rate (alphapos or alphaneg, respectively)
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

    Qs_init = rep_vector(0.0, 2);
    Qs = Qs_init;

    for (n in 1:nT)  {

      if (n == 1 || block_grp[n]!=block_grp[n-1]) {
        Qs = Qs_init;
      }
      if(block_grp[n]==0) {
        nu_out[n] = 0.0;
        continue;
      }

      // get choice of current trial (either 1 or 2)
      ci = (choice[n]==1 ? 2 : 1);

      // compute action probabilities
      nut = nu[n]*(Qs[2]-Qs[1]); // predicting upper bound
      nu_out[n] = 2 * numax[s] / (1 + exp(-nut)) - numax[s];

      // prediction error
      PE = (reward[n]) - Qs[ci];

      // value updating (learning)
      alpha = (PE >= 0) ? alphapos[n] : alphaneg[n];
      Qs[ci] += alpha * PE;

    } // trial loop

    return nu_out;
  }
    ',
    block = 'functions'
  )




### qlddm_2a_rho_qdiff_num_func ---------------------------------------------


qlddm_2a_rho_qdiff_num_func <-
  brms::stanvar(
    scode = '
  vector qlddm_2a_rho_qdiff_nu(vector block_grp, vector choice, vector reward, vector alphapos, vector alphaneg, vector rho, vector nu, vector numax) {
    int nT = size(choice);
    vector[nT] nu_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real PE;            // prediction error
    real nut;           // linear slope of random walk in trial t
    real alpha;         // effective learning rate (alphapos or alphaneg, respectively)
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

    Qs_init = rep_vector(0.0, 2);
    Qs = Qs_init;

    for (n in 1:nT)  {

      if (n == 1 || block_grp[n]!=block_grp[n-1]) {
        Qs = Qs_init;
      }
      if(block_grp[n]==0) {
        nu_out[n] = 0.0;
        continue;
      }

      // get choice of current trial (either 1 or 2)
      ci = (choice[n]==1 ? 2 : 1);

      // compute action probabilities
      nut = nu[n]*(Qs[2]-Qs[1]); // predicting upper bound
      nu_out[n] = 2 * numax[s] / (1 + exp(-nut)) - numax[s];

      // prediction error
      PE = (reward[n] * rho[n]) - Qs[ci];

      // value updating (learning)
      alpha = (PE >= 0) ? alphapos[n] : alphaneg[n];
      Qs[ci] += alpha * PE;

    } // trial loop

    return nu_out;
  }
    ',
    block = 'functions'
  )




### qlddm_2a_2rho_qdiff_num_func ---------------------------------------------


qlddm_2a_2rho_qdiff_num_func <-
  brms::stanvar(
    scode = '
  vector qlddm_2a_2rho_qdiff_nu(vector block_grp, vector choice, vector reward, vector alphapos, vector alphaneg, vector rhopos, vector rhoneg, vector nu, vector numax) {
    int nT = size(choice);
    vector[nT] nu_out;

    vector[2] Qs;       // expectation value
    vector[2] Qs_init;  // initial expectation values
    real PE;            // prediction error
    real rho;           // effective reward sensitivity (rhopos or rhoneg, respectively)
    real nut;           // linear slope of random walk in trial t
    real alpha;         // effective learning rate (alphapos or alphaneg, respectively)
    vector[2] Ps;       // probabilities based on the softmax of the Q values
    int ci;             // index of the choice for which the ev needs to be updated
                        // 1 corresponds to the "lower bound" (coded as 0)
                        // 2 corresponds to the "upper bound" (coded as 1)

    Qs_init = rep_vector(0.0, 2);
    Qs = Qs_init;

    for (n in 1:nT)  {

      if (n == 1 || block_grp[n]!=block_grp[n-1]) {
        Qs = Qs_init;
      }
      if(block_grp[n]==0) {
        nu_out[n] = 0.0;
        continue;
      }

      // get choice of current trial (either 1 or 2)
      ci = (choice[n]==1 ? 2 : 1);

      // compute action probabilities
      nut = nu[n]*(Qs[2]-Qs[1]); // predicting upper bound
      nu_out[n] = 2 * numax[s] / (1 + exp(-nut)) - numax[s];

      // prediction error
      rho = reward[n] >= 0 ? rhospos[n] : rhoneg[n];
      PE = (reward[n] * rho) - Qs[ci];

      // value updating (learning)
      alpha = (PE >= 0) ? alphapos[n] : alphaneg[n];
      Qs[ci] += alpha * PE;

    } // trial loop

    return nu_out;
  }
    ',
    block = 'functions'
  )





### qlddm_hmm_g_ep_qdiff_nu_func -------------------------------------------------------


qlddm_hmm_g_ep_qdiff_nu_func <-
  brms::stanvar(
    scode = '
    vector qlddm_hmm_g_ep_qdiff_nu(vector block_grp, vector choice, vector reward, vector gamma, vector ep, vector nu) {


    int nT = size(choice);

    vector[nT] nu_out;

    vector[2] Ps;  // prob of the states, 1 - left, 2 - right
    real P_O_S1;   // p(O|S1) - O = {A,R} given left
    real P_O_S2;   // p(O|S2) - O = {A,R} given right
    real choice_ep; // p(A|R)

    vector[2] Ps_init; // initial prob/belief of the two states
    Ps_init = reptor(0.5, 2);

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
      nu_out[n] = nu[n]*(Ps[2]-Ps[1]); // if choice is coded as [1, 2], then level two (2) is used as upper bound in bernoulli, so we need p(S2)

      // renew of emission prob: p(O|S1) p(O|S2); O is a pair between A(ction) and R(eward)
      // --> the probability of actually observing this outcome
      choice_ep = reward[n] >= 0 ? ep[n] : (1-ep[n]);
      P_O_S1 = 0.5 * ( (choice[n] == 1) ? choice_ep : (1-choice_ep) );
      P_O_S2 = 0.5 * ( (choice[n] == 2) ? choice_ep : (1-choice_ep) );

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


### qlddm_hmm_g_ep_qdiff_num_func -------------------------------------------------------


qlddm_hmm_g_ep_qdiff_nu_func <-
  brms::stanvar(
    scode = '
    vector qlddm_hmm_g_ep_qdiff_nu(vector block_grp, vector choice, vector reward, vector gamma, vector ep, vector nu, vector numax) {


    int nT = size(choice);

    vector[nT] nu_out;

    vector[2] Ps;  // prob of the states, 1 - left, 2 - right
    real P_O_S1;   // p(O|S1) - O = {A,R} given left
    real P_O_S2;   // p(O|S2) - O = {A,R} given right
    real choice_ep; // p(A|R)
    real nut;       // linear slope of the random walk

    vector[2] Ps_init; // initial prob/belief of the two states
    Ps_init = reptor(0.5, 2);

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
      nut = nu[n]*(Ps[2]-Ps[1]); // if choice is coded as [1, 2], then level two (2) is used as upper bound in bernoulli, so we need p(S2)
      nu_out[n] = 2 * numax[s] / (1 + exp(-nut)) - numax[s];

      // renew of emission prob: p(O|S1) p(O|S2); O is a pair between A(ction) and R(eward)
      // --> the probability of actually observing this outcome
      choice_ep = reward[n] >= 0 ? ep[n] : (1-ep[n]);
      P_O_S1 = 0.5 * ( (choice[n] == 1) ? choice_ep : (1-choice_ep) );
      P_O_S2 = 0.5 * ( (choice[n] == 2) ? choice_ep : (1-choice_ep) );

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


### qlddm_hmm_g_2ep_qdiff_nu_func -------------------------------------------------------


qlddm_hmm_g_2ep_qdiff_nu_func <-
  brms::stanvar(
    scode = '
    vector qlddm_hmm_g_2ep_qdiff_nu(vector block_grp, vector choice, vector reward, vector gamma, vector ep_pos, vector ep_neg) {


    int nT = size(choice);

    vector[nT] nu_out;

    vector[2] Ps;  // prob of the states, 1 - left, 2 - right
    real P_O_S1;   // p(O|S1) - O = {A,R} given left
    real P_O_S2;   // p(O|S2) - O = {A,R} given right
    real choice_ep; // p(A|R)


    vector[2] Ps_init; // initial prob/belief of the two states
    Ps_init = reptor(0.5, 2);

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
      nu_out[n] = nu[n]*(Ps[2]-Ps[1]); // if choice is coded as [1, 2], then level two (2) is used as upper bound in bernoulli, so we need p(S2)

      // renew of emission prob: p(O|S1) p(O|S2); O is a pair between A(ction) and R(eward)
      // --> the probability of actually observing this outcome
      choice_ep = reward[n] >= 0 ? ep_pos[n] : (1-ep_neg[n]);
      P_O_S1 = 0.5 * ( (choice[n] == 1) ? choice_ep : (1-choice_ep) );
      P_O_S2 = 0.5 * ( (choice[n] == 2) ? choice_ep : (1-choice_ep) );

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


### qlddm_hmm_g_2ep_qdiff_num_func -------------------------------------------------------


qlddm_hmm_g_2ep_qdiff_nu_func <-
  brms::stanvar(
    scode = '
    vector qlddm_hmm_g_2ep_qdiff_nu(vector block_grp, vector choice, vector reward, vector gamma, vector ep_pos, vector ep_neg, vector nu, vector numax) {


    int nT = size(choice);

    vector[nT] nu_out;

    vector[2] Ps;  // prob of the states, 1 - left, 2 - right
    real P_O_S1;   // p(O|S1) - O = {A,R} given left
    real P_O_S2;   // p(O|S2) - O = {A,R} given right
    real choice_ep; // p(A|R)
    real nu;        // linear slope of the random walk


    vector[2] Ps_init; // initial prob/belief of the two states
    Ps_init = reptor(0.5, 2);

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
      nut = nu[n]*(Ps[2]-Ps[1]); // if choice is coded as [1, 2], then level two (2) is used as upper bound in bernoulli, so we need p(S2)
      nu_out[n] = 2 * numax[s] / (1 + exp(-nut)) - numax[s];

      // renew of emission prob: p(O|S1) p(O|S2); O is a pair between A(ction) and R(eward)
      // --> the probability of actually observing this outcome
      choice_ep = reward[n] >= 0 ? ep_pos[n] : (1-ep_neg[n]);
      P_O_S1 = 0.5 * ( (choice[n] == 1) ? choice_ep : (1-choice_ep) );
      P_O_S2 = 0.5 * ( (choice[n] == 2) ? choice_ep : (1-choice_ep) );

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


# all stanvars ------------------------------------------------------------

all_stanvars <-
  list(
    hmm_rp_probs_func = hmm_rp_probs_func,
    hmm_rp_blm_probs_func = hmm_rp_blm_probs_func,
    ql_a_it_blm_probs_func = ql_a_it_blm_probs_func,
    ql_a_it_probs_func = ql_a_it_probs_func,
    ql_a_2it_probs_func = ql_a_2it_probs_func,
    ql_2a_it_probs_func = ql_2a_it_probs_func,
    ql_2a_2it_probs_func = ql_2a_2it_probs_func,
    ql_a_xi_probs_func = ql_a_xi_probs_func,
    ql_a_2xi_probs_func = ql_a_2xi_probs_func,
    ql_2a_xi_probs_func = ql_2a_xi_probs_func,
    ql_2a_2xi_probs_func = ql_2a_2xi_probs_func,
    ql_a_rho_probs_func = ql_a_rho_probs_func,
    ql_2a_rho_probs_func = ql_2a_rho_probs_func,
    ql_a_2rho_probs_func = ql_a_2rho_probs_func,
    ql_2a_2rho_probs_func = ql_2a_2rho_probs_func,
    ql_a_it_fu_probs_func = ql_a_it_fu_probs_func,
    ql_a_2it_fu_probs_func = ql_a_2it_fu_probs_func,
    ql_2a_it_fu_probs_func = ql_2a_it_fu_probs_func,
    ql_2a_2it_fu_probs_func = ql_2a_2it_fu_probs_func,
    ql_a_xi_fu_probs_func = ql_a_xi_fu_probs_func,
    ql_a_2xi_fu_probs_func = ql_a_2xi_fu_probs_func,
    ql_2a_xi_fu_probs_func = ql_2a_xi_fu_probs_func,
    ql_2a_2xi_fu_probs_func = ql_2a_2xi_fu_probs_func,
    ql_a_rho_fu_probs_func = ql_a_rho_fu_probs_func,
    ql_a_2rho_fu_probs_func = ql_a_2rho_fu_probs_func,
    ql_a_it_kfu_probs_func = ql_a_it_kfu_probs_func,
    ql_a_2it_kfu_probs_func = ql_a_2it_kfu_probs_func,
    ql_2a_it_kfu_probs_func = ql_2a_it_kfu_probs_func,
    ql_2a_2it_kfu_probs_func = ql_2a_2it_kfu_probs_func,
    ql_a_xi_kfu_probs_func = ql_a_xi_kfu_probs_func,
    ql_a_2xi_kfu_probs_func = ql_a_2xi_kfu_probs_func,
    ql_2a_xi_kfu_probs_func = ql_2a_xi_kfu_probs_func,
    ql_2a_2xi_kfu_probs_func = ql_2a_2xi_kfu_probs_func,
    ql_a_rho_kfu_probs_func = ql_a_rho_kfu_probs_func,
    ql_a_2rho_kfu_probs_func = ql_a_2rho_kfu_probs_func,
    ql_2a_2rho_fu_probs_func = ql_2a_2rho_fu_probs_func,
    qlddm_2a_qdiff_func = qlddm_2a_qdiff_func,
    qlddm_2a_qdiff_nu_func = qlddm_2a_qdiff_nu_func,
    qlddm_a_qdiff_nu_func = qlddm_a_qdiff_nu_func
  )

