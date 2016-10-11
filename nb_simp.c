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

struct arrayProb fn_a(struct arrayProb topic_prior_b,
		       struct arrayProb word_prior_c,
		       struct arrayNat z_d,
		       struct arrayNat w_e,
		       struct arrayNat doc_f,
		       unsigned int docUpdate_g)
{
  struct arrayProb arr_h;
  unsigned int zNew丏_i;
  arr_h.size = topic_prior_b.size;
  arr_h.data = ((double *)malloc((topic_prior_b.size * sizeof(double))));
  for (zNew丏_i = 0; zNew丏_i < topic_prior_b.size; zNew丏_i++)
  {
    unsigned int i_j;
    double acc_k;
    acc_k = 1;
    for (i_j = 0; i_j < topic_prior_b.size; i_j++)
    {
      unsigned int i丣_l;
      double acc_m;
      acc_m = 1;
      for (i丣_l = 0; i丣_l < word_prior_c.size; i丣_l++)
      {
        unsigned int i丙_n;
        unsigned int acc_o;
        unsigned int j_v;
        double acc_w;
        acc_o = 0;
        for (i丙_n = 0; i丙_n < w_e.size; i丙_n++)
        {
          struct SUSUV eq_p;
          unsigned int _q;
          struct SUSUV eq_r;
          struct SUSUV eq_s;
          struct SUSUV and_t;
          unsigned int _u;
          eq_p.index = (docUpdate_g == *(doc_f.data + i丙_n)) ? 0 : 1;
          if (eq_p.index == 0)
          {
            eq_r.index = (i_j == zNew丏_i) ? 0 : 1;
            eq_s.index = (i丣_l == *(w_e.data + i丙_n)) ? 0 : 1;
            and_t.index = !(eq_s.index == eq_r.index);
            if (and_t.index == 0)
            {
              _u = 1;
            }
            if (and_t.index == 1)
            {
              _u = 0;
            }
            _q = _u;
          }
          if (eq_p.index == 1)
          {
            _q = 0;
          }
          acc_o += _q;
        }
        acc_w = 1;
        for (j_v = 0; j_v < acc_o; j_v++)
        {
          unsigned int i丙_x;
          unsigned int acc_y;
          double p_a5;
          double p_a6;
          double logSumExp_a7;
          unsigned int i丙_a8;
          unsigned int acc_a9;
          double p_ae;
          double logSumExp_af;
          unsigned int i丙_ag;
          unsigned int acc_ah;
          double p_am;
          unsigned int i丙_an;
          double acc_ao;
          double logSumExp_ap;
          double recip_aq;
          unsigned int i_ar;
          double acc_as;
          double recip_bc;
          acc_y = 0;
          for (i丙_x = 0; i丙_x < w_e.size; i丙_x++)
          {
            struct SUSUV eq_z;
            unsigned int _a0;
            struct SUSUV eq_a1;
            struct SUSUV eq_a2;
            struct SUSUV and_a3;
            unsigned int _a4;
            eq_z.index = (*(doc_f.data + i丙_x) == docUpdate_g) ? 0 : 1;
            if (eq_z.index == 0)
            {
              _a0 = 0;
            }
            if (eq_z.index == 1)
            {
              eq_a1.index = (i_j == *(z_d.data + *(doc_f.data + i丙_x))) ? 0 : 1;
              eq_a2.index = (i丣_l == *(w_e.data + i丙_x)) ? 0 : 1;
              and_a3.index = !(eq_a2.index == eq_a1.index);
              if (and_a3.index == 0)
              {
                _a4 = 1;
              }
              if (and_a3.index == 1)
              {
                _a4 = 0;
              }
              _a0 = _a4;
            }
            acc_y += _a0;
          }
          p_a5 = log1p((acc_y - 1));
          p_a6 = log1p((j_v - 1));
          logSumExp_a7 = logSumExp3(p_a5,p_a6,*(word_prior_c.data + i丣_l));
          acc_a9 = 0;
          for (i丙_a8 = 0; i丙_a8 < z_d.size; i丙_a8++)
          {
            struct SUSUV eq_aa;
            unsigned int _ab;
            struct SUSUV eq_ac;
            unsigned int _ad;
            eq_aa.index = (i丙_a8 == docUpdate_g) ? 0 : 1;
            if (eq_aa.index == 0)
            {
              _ab = 0;
            }
            if (eq_aa.index == 1)
            {
              eq_ac.index = (zNew丏_i == *(z_d.data + i丙_a8)) ? 0 : 1;
              if (eq_ac.index == 0)
              {
                _ad = 1;
              }
              if (eq_ac.index == 1)
              {
                _ad = 0;
              }
              _ab = _ad;
            }
            acc_a9 += _ab;
          }
          p_ae = log1p((acc_a9 - 1));
          logSumExp_af = logSumExp2(p_ae,*(topic_prior_b.data + zNew丏_i));
          acc_ah = 0;
          for (i丙_ag = 0; i丙_ag < z_d.size; i丙_ag++)
          {
            struct SUSUV eq_ai;
            unsigned int _aj;
            struct SUSUV less_ak;
            unsigned int _al;
            eq_ai.index = (i丙_ag == docUpdate_g) ? 0 : 1;
            if (eq_ai.index == 0)
            {
              _aj = 0;
            }
            if (eq_ai.index == 1)
            {
              less_ak.index = (*(z_d.data + i丙_ag) < 0) ? 0 : 1;
              if (less_ak.index == 0)
              {
                _al = 0;
              }
              if (less_ak.index == 1)
              {
                _al = 1;
              }
              _aj = _al;
            }
            acc_ah += _aj;
          }
          p_am = log1p((acc_ah - 1));
          acc_ao = 0;
          for (i丙_an = 0; i丙_an < topic_prior_b.size; i丙_an++)
          {
            acc_ao += *(topic_prior_b.data + i丙_an);
          }
          logSumExp_ap = logSumExp2(p_am,acc_ao);
          recip_aq = -logSumExp_ap;
          acc_as = 1;
          for (i_ar = 0; i_ar < topic_prior_b.size; i_ar++)
          {
            unsigned int i丙_at;
            unsigned int acc_au;
            unsigned int i丣_az;
            double acc_b0;
            acc_au = 0;
            for (i丙_at = 0; i丙_at < w_e.size; i丙_at++)
            {
              struct SUSUV eq_av;
              unsigned int _aw;
              struct SUSUV eq_ax;
              unsigned int _ay;
              eq_av.index = (docUpdate_g == *(doc_f.data + i丙_at)) ? 0 : 1;
              if (eq_av.index == 0)
              {
                eq_ax.index = (i_ar == zNew丏_i) ? 0 : 1;
                if (eq_ax.index == 0)
                {
                  _ay = 1;
                }
                if (eq_ax.index == 1)
                {
                  _ay = 0;
                }
                _aw = _ay;
              }
              if (eq_av.index == 1)
              {
                _aw = 0;
              }
              acc_au += _aw;
            }
            acc_b0 = 1;
            for (i丣_az = 0; i丣_az < acc_au; i丣_az++)
            {
              unsigned int i丙_b1;
              unsigned int acc_b2;
              double p_b7;
              double p_b8;
              unsigned int i丙_b9;
              double acc_ba;
              double logSumExp_bb;
              acc_b2 = 0;
              for (i丙_b1 = 0; i丙_b1 < w_e.size; i丙_b1++)
              {
                struct SUSUV eq_b3;
                unsigned int _b4;
                struct SUSUV eq_b5;
                unsigned int _b6;
                eq_b3.index = (*(doc_f.data + i丙_b1) == docUpdate_g) ? 0 : 1;
                if (eq_b3.index == 0)
                {
                  _b4 = 0;
                }
                if (eq_b3.index == 1)
                {
                  eq_b5.index = (i_ar == *(z_d.data + *(doc_f.data + i丙_b1))) ? 0 : 1;
                  if (eq_b5.index == 0)
                  {
                    _b6 = 1;
                  }
                  if (eq_b5.index == 1)
                  {
                    _b6 = 0;
                  }
                  _b4 = _b6;
                }
                acc_b2 += _b4;
              }
              p_b7 = log1p((acc_b2 - 1));
              p_b8 = log1p((i丣_az - 1));
              acc_ba = 0;
              for (i丙_b9 = 0; i丙_b9 < word_prior_c.size; i丙_b9++)
              {
                acc_ba += *(word_prior_c.data + i丙_b9);
              }
              logSumExp_bb = logSumExp3(p_b7,p_b8,acc_ba);
              acc_b0 += logSumExp_bb;
            }
            acc_as += acc_b0;
          }
          recip_bc = -acc_as;
          acc_w += (logSumExp_af + (recip_aq + (recip_bc + logSumExp_a7)));
        }
        acc_m += acc_w;
      }
      acc_k += acc_m;
    }
    *(arr_h.data + zNew丏_i) = acc_k;
  }
  return arr_hi;
}

