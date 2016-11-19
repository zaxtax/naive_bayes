#include <stdlib.h>
#include <stdio.h>
#include <math.h>

struct arrayProb {
  int size; double * data;
};

struct arrayNat {
  int size; unsigned int * data;
};

struct SUSUV {
  int index;
};

double logSumExp3(double _a, double _b, double _c)
{
  return (_a > _b) ? (_a > _c) ? (_a + log1p(((expm1((_c - _a)) + expm1((_b - _a))) + 2))) : (_c + log1p(((expm1((_b - _c)) + expm1((_a - _c))) + 2))) : (_b > _c) ? (_b + log1p(((expm1((_c - _b)) + expm1((_a - _b))) + 2))) : (_c + log1p(((expm1((_b - _c)) + expm1((_a - _c))) + 2)));
}

double logSumExp2(double _a, double _b)
{
  return (_a > _b) ? (_a + log1p((expm1((_b - _a)) + 1))) : (_b + log1p((expm1((_a - _b)) + 1)));
}

struct arrayProb gibbsC(struct arrayProb topic_prior_a, struct arrayProb word_prior_b, struct arrayNat z_c, struct arrayNat w_d, struct arrayNat doc_e, unsigned int docUpdate_f)
{
  struct arrayProb arr_g;
  unsigned int zNew8_h;
  arr_g.size = topic_prior_a.size;
  arr_g.data = ((double *)malloc((topic_prior_a.size * sizeof(double))));
  for (zNew8_h = 0; zNew8_h < topic_prior_a.size; zNew8_h++)
  {
    unsigned int i_i;
    double acc_j;
    acc_j = 1;
    for (i_i = 0; i_i < topic_prior_a.size; i_i++)
    {
      unsigned int i18_k;
      double acc_l;
      acc_l = 1;
      for (i18_k = 0; i18_k < word_prior_b.size; i18_k++)
      {
        unsigned int i13_m;
        unsigned int acc_n;
        unsigned int j_u;
        double acc_v;
        acc_n = 0;
        for (i13_m = 0; i13_m < w_d.size; i13_m++)
        {
          struct SUSUV eq_o;
          unsigned int _p;
          struct SUSUV eq_q;
          struct SUSUV eq_r;
          struct SUSUV and_s;
          unsigned int _t;
          eq_o.index = (docUpdate_f == *(doc_e.data + i13_m)) ? 0 : 1;
          if (eq_o.index == 0)
          {
            eq_q.index = (i_i == zNew8_h) ? 0 : 1;
            eq_r.index = (i18_k == *(w_d.data + i13_m)) ? 0 : 1;
            and_s.index = !(eq_r.index == eq_q.index);
            if (and_s.index == 0)
            {
              _t = 1;
            }
            if (and_s.index == 1)
            {
              _t = 0;
            }
            _p = _t;
          }
          if (eq_o.index == 1)
          {
            _p = 0;
          }
          acc_n += _p;
        }
        acc_v = 1;
        for (j_u = 0; j_u < acc_n; j_u++)
        {
          unsigned int i13_w;
          unsigned int acc_x;
          double p_a4;
          double p_a5;
          double logSumExp_a6;
          unsigned int i13_a7;
          unsigned int acc_a8;
          double p_ad;
          double logSumExp_ae;
          unsigned int i13_af;
          unsigned int acc_ag;
          double p_al;
          unsigned int i13_am;
          double acc_an;
          double logSumExp_ao;
          double recip_ap;
          unsigned int i_aq;
          double acc_ar;
          double recip_bh;
          acc_x = 0;
          for (i13_w = 0; i13_w < w_d.size; i13_w++)
          {
            struct SUSUV eq_y;
            unsigned int _z;
            struct SUSUV eq_a0;
            struct SUSUV eq_a1;
            struct SUSUV and_a2;
            unsigned int _a3;
            eq_y.index = (*(doc_e.data + i13_w) == docUpdate_f) ? 0 : 1;
            if (eq_y.index == 0)
            {
              _z = 0;
            }
            if (eq_y.index == 1)
            {
              eq_a0.index = (i_i == *(z_c.data + *(doc_e.data + i13_w))) ? 0 : 1;
              eq_a1.index = (i18_k == *(w_d.data + i13_w)) ? 0 : 1;
              and_a2.index = !(eq_a1.index == eq_a0.index);
              if (and_a2.index == 0)
              {
                _a3 = 1;
              }
              if (and_a2.index == 1)
              {
                _a3 = 0;
              }
              _z = _a3;
            }
            acc_x += _z;
          }
          p_a4 = log1p((acc_x - 1));
          p_a5 = log1p((j_u - 1));
          logSumExp_a6 = logSumExp3(p_a4,p_a5,*(word_prior_b.data + i18_k));
          acc_a8 = 0;
          for (i13_a7 = 0; i13_a7 < z_c.size; i13_a7++)
          {
            struct SUSUV eq_a9;
            unsigned int _aa;
            struct SUSUV eq_ab;
            unsigned int _ac;
            eq_a9.index = (i13_a7 == docUpdate_f) ? 0 : 1;
            if (eq_a9.index == 0)
            {
              _aa = 0;
            }
            if (eq_a9.index == 1)
            {
              eq_ab.index = (zNew8_h == *(z_c.data + i13_a7)) ? 0 : 1;
              if (eq_ab.index == 0)
              {
                _ac = 1;
              }
              if (eq_ab.index == 1)
              {
                _ac = 0;
              }
              _aa = _ac;
            }
            acc_a8 += _aa;
          }
          p_ad = log1p((acc_a8 - 1));
          logSumExp_ae = logSumExp2(p_ad,*(topic_prior_a.data + zNew8_h));
          acc_ag = 0;
          for (i13_af = 0; i13_af < z_c.size; i13_af++)
          {
            struct SUSUV eq_ah;
            unsigned int _ai;
            struct SUSUV less_aj;
            unsigned int _ak;
            eq_ah.index = (i13_af == docUpdate_f) ? 0 : 1;
            if (eq_ah.index == 0)
            {
              _ai = 0;
            }
            if (eq_ah.index == 1)
            {
              less_aj.index = (*(z_c.data + i13_af) < 0) ? 0 : 1;
              if (less_aj.index == 0)
              {
                _ak = 0;
              }
              if (less_aj.index == 1)
              {
                _ak = 1;
              }
              _ai = _ak;
            }
            acc_ag += _ai;
          }
          p_al = log1p((acc_ag - 1));
          acc_an = 0;
          for (i13_am = 0; i13_am < topic_prior_a.size; i13_am++)
          {
            acc_an += *(topic_prior_a.data + i13_am);
          }
          logSumExp_ao = logSumExp2(p_al,acc_an);
          recip_ap = -logSumExp_ao;
          acc_ar = 1;
          for (i_aq = 0; i_aq < topic_prior_a.size; i_aq++)
          {
            unsigned int i13_as;
            unsigned int acc_at;
            unsigned int i18_b1;
            double acc_b2;
            acc_at = 0;
            for (i13_as = 0; i13_as < w_d.size; i13_as++)
            {
              struct SUSUV eq_au;
              unsigned int _av;
              struct SUSUV less_ax;
              struct SUSUV eq_ay;
              struct SUSUV and_az;
              unsigned int _b0;
              eq_au.index = (docUpdate_f == *(doc_e.data + i13_as)) ? 0 : 1;
              if (eq_au.index == 0)
              {
                less_ax.index = (*(w_d.data + i13_as) < 0) ? 0 : 1;
                less_ax.index = (less_ax.index == 1) ? 0 : 1;
                eq_ay.index = (i_aq == zNew8_h) ? 0 : 1;
                and_az.index = !(eq_ay.index == less_ax.index);
                if (and_az.index == 0)
                {
                  _b0 = 1;
                }
                if (and_az.index == 1)
                {
                  _b0 = 0;
                }
                _av = _b0;
              }
              if (eq_au.index == 1)
              {
                _av = 0;
              }
              acc_at += _av;
            }
            acc_b2 = 1;
            for (i18_b1 = 0; i18_b1 < acc_at; i18_b1++)
            {
              unsigned int i13_b3;
              unsigned int acc_b4;
              double p_bc;
              double p_bd;
              unsigned int i13_be;
              double acc_bf;
              double logSumExp_bg;
              acc_b4 = 0;
              for (i13_b3 = 0; i13_b3 < w_d.size; i13_b3++)
              {
                struct SUSUV eq_b5;
                unsigned int _b6;
                struct SUSUV less_b8;
                struct SUSUV eq_b9;
                struct SUSUV and_ba;
                unsigned int _bb;
                eq_b5.index = (*(doc_e.data + i13_b3) == docUpdate_f) ? 0 : 1;
                if (eq_b5.index == 0)
                {
                  _b6 = 0;
                }
                if (eq_b5.index == 1)
                {
                  less_b8.index = (*(w_d.data + i13_b3) < 0) ? 0 : 1;
                  less_b8.index = (less_b8.index == 1) ? 0 : 1;
                  eq_b9.index = (i_aq == *(z_c.data + *(doc_e.data + i13_b3))) ? 0 : 1;
                  and_ba.index = !(eq_b9.index == less_b8.index);
                  if (and_ba.index == 0)
                  {
                    _bb = 1;
                  }
                  if (and_ba.index == 1)
                  {
                    _bb = 0;
                  }
                  _b6 = _bb;
                }
                acc_b4 += _b6;
              }
              p_bc = log1p((acc_b4 - 1));
              p_bd = log1p((i18_b1 - 1));
              acc_bf = 0;
              for (i13_be = 0; i13_be < word_prior_b.size; i13_be++)
              {
                acc_bf += *(word_prior_b.data + i13_be);
              }
              logSumExp_bg = logSumExp3(p_bc,p_bd,acc_bf);
              acc_b2 += logSumExp_bg;
            }
            acc_ar += acc_b2;
          }
          recip_bh = -acc_ar;
          acc_v += (logSumExp_ae + (recip_ap + (recip_bh + logSumExp_a6)));
        }
        acc_l += acc_v;
      }
      acc_j += acc_l;
    }
    *(arr_g.data + zNew8_h) = acc_j;
  }
  return arr_g;
}

struct arrayProb* gibbsC_shim(struct arrayProb* topic_prior_b,
			    struct arrayProb* word_prior_c,
			    struct arrayNat* z_d,
			    struct arrayNat* w_e,
			    struct arrayNat* doc_f,
			    unsigned int docUpdate_g)
{
  struct arrayProb* res = (struct arrayProb*)malloc(sizeof(struct arrayProb*));
  *res = gibbsC(*topic_prior_b, *word_prior_c, *z_d, *w_e, *doc_f, docUpdate_g);
  return res;
}
