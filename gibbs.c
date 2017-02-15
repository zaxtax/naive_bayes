#include "nb_simp.c"

struct arrayProb* gibbsC_shim(struct array_prob* topic_prior_b,
                struct array_prob* word_prior_c,
                struct array_nat* z_d,
                struct array_nat* w_e,
                struct array_nat* doc_f,
                unsigned int docUpdate_g)
{
  struct array_prob* res = (struct array_prob*)malloc(sizeof(struct array_prob));
  *res = gibbsC(*topic_prior_b, *word_prior_c, *z_d, *w_e, *doc_f, docUpdate_g);
  return res;
}

