#include <stdlib.h>
#include <stdio.h>
#include <math.h>

struct array_prob {
  int size; double * data;
};

struct array_nat {
  int size; unsigned int * data;
};

struct mdata_nat {
  double weight; unsigned int sample;
};

struct dat_DDV {
  int index;
};

double logSumExp2(double _a, double _b)
{
  return (_a > _b) ? (_a + log1p((expm1((_b - _a)) + 1))) : (_b + log1p((expm1((_a - _b)) + 1)));
}

double logSumExp3(double _a, double _b, double _c)
{
  return (_a > _b) ? (_a > _c) ? (_a + log1p(((expm1((_c - _a)) + expm1((_b - _a))) + 2))) : (_c + log1p(((expm1((_b - _c)) + expm1((_a - _c))) + 2))) : (_b > _c) ? (_b + log1p(((expm1((_c - _b)) + expm1((_a - _b))) + 2))) : (_c + log1p(((expm1((_b - _c)) + expm1((_a - _c))) + 2)));
}

struct mdata_nat gibbsC(struct array_prob topic_prior_a, struct array_prob word_prior_b, struct array_nat z_c, struct array_nat w_d, struct array_nat doc_e, unsigned int docUpdate_f)
{
  struct mdata_nat out_g;
  unsigned int _h;
  struct array_nat arr_i;
  unsigned int _j;
  struct array_nat arr_k;
  unsigned int _l;
  unsigned int _m;
  unsigned int _n;
  unsigned int iu19993_o;
  unsigned int acc_p;
  double _a3;
  unsigned int _a4;
  double p_a5;
  unsigned int _a6;
  struct array_prob arr_a7;
  double _a8;
  unsigned int _a9;
  unsigned int _aa;
  struct array_prob summate_arr_ab;
  double maxV_ac;
  double sum_ae;
  unsigned int maxI_ad;
  unsigned int iu19993_af;
  unsigned int _aj;
  struct array_prob arr_ak;
  double _al;
  unsigned int _am;
  unsigned int _an;
  struct array_prob summate_arr_ao;
  double maxV_ap;
  double sum_ar;
  unsigned int maxI_aq;
  unsigned int iu19993_as;
  double _aw;
  double _ax;
  double _ay;
  double _az;
  double _b0;
  struct array_prob _b1;
  unsigned int zNewu19983_b2;
  struct array_prob _h1;
  int it_h2;
  double ws_h3;
  double max_h4;
  double r_h5;
  arr_i = w_d;
  _h = arr_i.size;
  arr_k = z_c;
  _j = arr_k.size;
  _m = 0;
  _n = _j;
  /* ---------- Begin Summate ---------- */
  acc_p = 0;
  for (iu19993_o = _m; iu19993_o < _n; iu19993_o++)
  {
    unsigned int _q;
    unsigned int _r;
    struct array_nat arr_s;
    unsigned int _t;
    struct dat_DDV _u;
    unsigned int _v;
    unsigned int _w;
    unsigned int _x;
    struct dat_DDV _y;
    struct dat_DDV _z;
    unsigned int _a0;
    unsigned int _a1;
    struct dat_DDV _a2;
    arr_s = z_c;
    _t = iu19993_o;
    _r = arr_s.data[_t];
    _v = _r;
    _w = 0;
    _u.index = (_v < _w) ? 0 : 1;
    _y = _u;
     if ((_y.index == 0))
     {
       _x = 0;
     }
     else
      if ((_y.index == 1))
      {
        _x = 1;
      }
    _a0 = iu19993_o;
    _a1 = docUpdate_f;
    _z.index = (_a0 == _a1) ? 0 : 1;
    _a2 = _z;
     if ((_a2.index == 0))
     {
       _q = 0;
     }
     else
      if ((_a2.index == 1))
      {
        _q = _x;
      }
    acc_p += _q;
    _l = acc_p;
  }
  /* ----------- End Summate ----------- */
  _a4 = _l;
  p_a5 = log1p((_a4 - 1));
  _a3 = p_a5;
  arr_a7 = word_prior_b;
  _a6 = arr_a7.size;
  _a9 = 0;
  _aa = _a6;
  /* ---------- Begin Summate ---------- */
  summate_arr_ab.size = (_aa - _a9);
  summate_arr_ab.data = ((double *)malloc((summate_arr_ab.size * sizeof(double))));
  for (iu19993_af = 0; iu19993_af < summate_arr_ab.size; iu19993_af++)
  {
    double _ag;
    struct array_prob arr_ah;
    unsigned int _ai;
    arr_ah = word_prior_b;
    _ai = iu19993_af;
    _ag = arr_ah.data[_ai];
    summate_arr_ab.data[iu19993_af] = _ag;
    if (((maxV_ac < _ag) || (iu19993_af == 0)))
    {
      maxV_ac = _ag;
      maxI_ad = iu19993_af;
    }
  }
  sum_ae = 0.0;
  for (iu19993_af = 0; iu19993_af < summate_arr_ab.size; iu19993_af++)
  {
    if ((iu19993_af != maxI_ad))
     sum_ae += exp((summate_arr_ab.data[iu19993_af] - maxV_ac));
  }
  _a8 = (maxV_ac + log(sum_ae));
  free(summate_arr_ab.data);
  /* ----------- End Summate ----------- */
  arr_ak = topic_prior_a;
  _aj = arr_ak.size;
  _am = 0;
  _an = _aj;
  /* ---------- Begin Summate ---------- */
  summate_arr_ao.size = (_an - _am);
  summate_arr_ao.data = ((double *)malloc((summate_arr_ao.size * sizeof(double))));
  for (iu19993_as = 0; iu19993_as < summate_arr_ao.size; iu19993_as++)
  {
    double _at;
    struct array_prob arr_au;
    unsigned int _av;
    arr_au = topic_prior_a;
    _av = iu19993_as;
    _at = arr_au.data[_av];
    summate_arr_ao.data[iu19993_as] = _at;
    if (((maxV_ap < _at) || (iu19993_as == 0)))
    {
      maxV_ap = _at;
      maxI_aq = iu19993_as;
    }
  }
  sum_ar = 0.0;
  for (iu19993_as = 0; iu19993_as < summate_arr_ao.size; iu19993_as++)
  {
    if ((iu19993_as != maxI_aq))
     sum_ar += exp((summate_arr_ao.data[iu19993_as] - maxV_ap));
  }
  _al = (maxV_ap + log(sum_ar));
  free(summate_arr_ao.data);
  /* ----------- End Summate ----------- */
  _ax = _a3;
  _ay = _al;
  _aw = logSumExp2(_ax,_ay);
  _b0 = _aw;
  _az = (-_b0);
  _b1.size = _aj;
  _b1.data = ((double *)malloc((_b1.size * sizeof(double))));
  /* ----------- Create Array ----------- */
  for (zNewu19983_b2 = 0; zNewu19983_b2 < _b1.size; zNewu19983_b2++)
  {
    double _b3;
    unsigned int _b4;
    unsigned int _b5;
    unsigned int i_b8;
    double t_b6;
    double c_b7;
    double _di;
    double _dj;
    double _dk;
    struct array_prob arr_dl;
    unsigned int _dm;
    unsigned int _dn;
    unsigned int _do;
    unsigned int _dp;
    unsigned int iu19993_dq;
    unsigned int acc_dr;
    double _e5;
    unsigned int _e6;
    double p_e7;
    double _e8;
    double _e9;
    double _ea;
    double _eb;
    unsigned int _ec;
    unsigned int _ed;
    unsigned int i_eg;
    double t_ee;
    double c_ef;
    double _gx;
    double _gy;
    double _gz;
    double _h0;
    _b4 = 0;
    _b5 = _aj;
    /* ---------- Begin Product ---------- */
    t_b6 = 0.0;
    c_b7 = 0.0;
    for (i_b8 = _b4; i_b8 < _b5; i_b8++)
    {
      double x_b9;
      double y_ba;
      double z_bb;
      unsigned int _bc;
      unsigned int _bd;
      unsigned int _be;
      unsigned int iu19993_bf;
      unsigned int acc_bg;
      double _c8;
      unsigned int _c9;
      double p_ca;
      struct dat_DDV _cb;
      unsigned int _cc;
      unsigned int _cd;
      unsigned int _ce;
      unsigned int _cf;
      unsigned int _cg;
      unsigned int iu19993_ch;
      unsigned int acc_ci;
      unsigned int _d4;
      unsigned int _d5;
      unsigned int iu20003_d8;
      double t_d6;
      double c_d7;
      _bd = 0;
      _be = _h;
      /* ---------- Begin Summate ---------- */
      acc_bg = 0;
      for (iu19993_bf = _bd; iu19993_bf < _be; iu19993_bf++)
      {
        unsigned int _bh;
        unsigned int _bi;
        struct array_nat arr_bj;
        unsigned int _bk;
        unsigned int _bl;
        struct array_nat arr_bm;
        unsigned int _bn;
        struct dat_DDV _bo;
        unsigned int _bp;
        unsigned int _bq;
        unsigned int _br;
        struct array_nat arr_bs;
        unsigned int _bt;
        struct dat_DDV _bu;
        unsigned int _bv;
        unsigned int _bw;
        struct dat_DDV _bx;
        struct dat_DDV not_by;
        struct dat_DDV _bz;
        struct dat_DDV _c0;
        struct dat_DDV _c1;
        unsigned int _c2;
        struct dat_DDV _c3;
        struct dat_DDV _c4;
        unsigned int _c5;
        unsigned int _c6;
        struct dat_DDV _c7;
        arr_bj = doc_e;
        _bk = iu19993_bf;
        _bi = arr_bj.data[_bk];
        arr_bm = z_c;
        _bn = _bi;
        _bl = arr_bm.data[_bn];
        _bp = i_b8;
        _bq = _bl;
        _bo.index = (_bp == _bq) ? 0 : 1;
        arr_bs = w_d;
        _bt = iu19993_bf;
        _br = arr_bs.data[_bt];
        _bv = _br;
        _bw = 0;
        _bu.index = (_bv < _bw) ? 0 : 1;
        not_by = _bu;
        not_by.index = (not_by.index == 1) ? 0 : 1;
        _c0 = _bx;
        _c1 = _bo;
        _bz.index = (!(_c1.index == _c0.index));
        _c3 = _bz;
         if ((_c3.index == 0))
         {
           _c2 = 1;
         }
         else
          if ((_c3.index == 1))
          {
            _c2 = 0;
          }
        _c5 = _bi;
        _c6 = docUpdate_f;
        _c4.index = (_c5 == _c6) ? 0 : 1;
        _c7 = _c4;
         if ((_c7.index == 0))
         {
           _bh = 0;
         }
         else
          if ((_c7.index == 1))
          {
            _bh = _c2;
          }
        acc_bg += _bh;
        _bc = acc_bg;
      }
      /* ----------- End Summate ----------- */
      _c9 = _bc;
      p_ca = log1p((_c9 - 1));
      _c8 = p_ca;
      _cc = i_b8;
      _cd = zNewu19983_b2;
      _cb.index = (_cc == _cd) ? 0 : 1;
      _cf = 0;
      _cg = _h;
      /* ---------- Begin Summate ---------- */
      acc_ci = 0;
      for (iu19993_ch = _cf; iu19993_ch < _cg; iu19993_ch++)
      {
        unsigned int _cj;
        unsigned int _ck;
        struct array_nat arr_cl;
        unsigned int _cm;
        struct dat_DDV _cn;
        unsigned int _co;
        unsigned int _cp;
        struct dat_DDV _cq;
        struct dat_DDV not_cr;
        struct dat_DDV _cs;
        struct dat_DDV _ct;
        struct dat_DDV _cu;
        unsigned int _cv;
        struct dat_DDV _cw;
        unsigned int _cx;
        struct array_nat arr_cy;
        unsigned int _cz;
        struct dat_DDV _d0;
        unsigned int _d1;
        unsigned int _d2;
        struct dat_DDV _d3;
        arr_cl = w_d;
        _cm = iu19993_ch;
        _ck = arr_cl.data[_cm];
        _co = _ck;
        _cp = 0;
        _cn.index = (_co < _cp) ? 0 : 1;
        not_cr = _cn;
        not_cr.index = (not_cr.index == 1) ? 0 : 1;
        _ct = _cq;
        _cu = _cb;
        _cs.index = (!(_cu.index == _ct.index));
        _cw = _cs;
         if ((_cw.index == 0))
         {
           _cv = 1;
         }
         else
          if ((_cw.index == 1))
          {
            _cv = 0;
          }
        arr_cy = doc_e;
        _cz = iu19993_ch;
        _cx = arr_cy.data[_cz];
        _d1 = docUpdate_f;
        _d2 = _cx;
        _d0.index = (_d1 == _d2) ? 0 : 1;
        _d3 = _d0;
         if ((_d3.index == 0))
         {
           _cj = _cv;
         }
         else
          if ((_d3.index == 1))
          {
            _cj = 0;
          }
        acc_ci += _cj;
        _ce = acc_ci;
      }
      /* ----------- End Summate ----------- */
      _d4 = 0;
      _d5 = _ce;
      /* ---------- Begin Product ---------- */
      t_d6 = 0.0;
      c_d7 = 0.0;
      for (iu20003_d8 = _d4; iu20003_d8 < _d5; iu20003_d8++)
      {
        double x_d9;
        double y_da;
        double z_db;
        double _dc;
        unsigned int _dd;
        double p_de;
        double _df;
        double _dg;
        double _dh;
        _dd = iu20003_d8;
        p_de = log1p((_dd - 1));
        _dc = p_de;
        _df = _c8;
        _dg = _dc;
        _dh = _a8;
        x_d9 = logSumExp3(_df,_dg,_dh);
        y_da = (x_d9 - c_d7);
        z_db = (t_d6 + y_da);
        c_d7 = ((z_db - t_d6) - y_da);
        t_d6 = z_db;
      }
      x_b9 = t_d6;
      /* ----------- End Product ----------- */
      y_ba = (x_b9 - c_b7);
      z_bb = (t_b6 + y_ba);
      c_b7 = ((z_bb - t_b6) - y_ba);
      t_b6 = z_bb;
    }
    _b3 = t_b6;
    /* ----------- End Product ----------- */
    _dj = _b3;
    _di = (-_dj);
    arr_dl = topic_prior_a;
    _dm = zNewu19983_b2;
    _dk = arr_dl.data[_dm];
    _do = 0;
    _dp = _j;
    /* ---------- Begin Summate ---------- */
    acc_dr = 0;
    for (iu19993_dq = _do; iu19993_dq < _dp; iu19993_dq++)
    {
      unsigned int _ds;
      unsigned int _dt;
      struct array_nat arr_du;
      unsigned int _dv;
      struct dat_DDV _dw;
      unsigned int _dx;
      unsigned int _dy;
      unsigned int _dz;
      struct dat_DDV _e0;
      struct dat_DDV _e1;
      unsigned int _e2;
      unsigned int _e3;
      struct dat_DDV _e4;
      arr_du = z_c;
      _dv = iu19993_dq;
      _dt = arr_du.data[_dv];
      _dx = zNewu19983_b2;
      _dy = _dt;
      _dw.index = (_dx == _dy) ? 0 : 1;
      _e0 = _dw;
       if ((_e0.index == 0))
       {
         _dz = 1;
       }
       else
        if ((_e0.index == 1))
        {
          _dz = 0;
        }
      _e2 = iu19993_dq;
      _e3 = docUpdate_f;
      _e1.index = (_e2 == _e3) ? 0 : 1;
      _e4 = _e1;
       if ((_e4.index == 0))
       {
         _ds = 0;
       }
       else
        if ((_e4.index == 1))
        {
          _ds = _dz;
        }
      acc_dr += _ds;
      _dn = acc_dr;
    }
    /* ----------- End Summate ----------- */
    _e6 = _dn;
    p_e7 = log1p((_e6 - 1));
    _e5 = p_e7;
    _e9 = _e5;
    _ea = _dk;
    _e8 = logSumExp2(_e9,_ea);
    _ec = 0;
    _ed = _aj;
    /* ---------- Begin Product ---------- */
    t_ee = 0.0;
    c_ef = 0.0;
    for (i_eg = _ec; i_eg < _ed; i_eg++)
    {
      double x_eh;
      double y_ei;
      double z_ej;
      struct dat_DDV _ek;
      unsigned int _el;
      unsigned int _em;
      unsigned int _en;
      unsigned int _eo;
      unsigned int iu20003_er;
      double t_ep;
      double c_eq;
      _el = i_eg;
      _em = zNewu19983_b2;
      _ek.index = (_el == _em) ? 0 : 1;
      _en = 0;
      _eo = _a6;
      /* ---------- Begin Product ---------- */
      t_ep = 0.0;
      c_eq = 0.0;
      for (iu20003_er = _en; iu20003_er < _eo; iu20003_er++)
      {
        double x_es;
        double y_et;
        double z_eu;
        double _ev;
        struct array_prob arr_ew;
        unsigned int _ex;
        unsigned int _ey;
        unsigned int _ez;
        unsigned int _f0;
        unsigned int iu19993_f1;
        unsigned int acc_f2;
        double _fs;
        unsigned int _ft;
        double p_fu;
        unsigned int _fv;
        unsigned int _fw;
        unsigned int _fx;
        unsigned int iu19993_fy;
        unsigned int acc_fz;
        unsigned int _gj;
        unsigned int _gk;
        unsigned int j_gn;
        double t_gl;
        double c_gm;
        arr_ew = word_prior_b;
        _ex = iu20003_er;
        _ev = arr_ew.data[_ex];
        _ez = 0;
        _f0 = _h;
        /* ---------- Begin Summate ---------- */
        acc_f2 = 0;
        for (iu19993_f1 = _ez; iu19993_f1 < _f0; iu19993_f1++)
        {
          unsigned int _f3;
          unsigned int _f4;
          struct array_nat arr_f5;
          unsigned int _f6;
          struct dat_DDV _f7;
          unsigned int _f8;
          unsigned int _f9;
          unsigned int _fa;
          struct array_nat arr_fb;
          unsigned int _fc;
          unsigned int _fd;
          struct array_nat arr_fe;
          unsigned int _ff;
          struct dat_DDV _fg;
          unsigned int _fh;
          unsigned int _fi;
          struct dat_DDV _fj;
          struct dat_DDV _fk;
          struct dat_DDV _fl;
          unsigned int _fm;
          struct dat_DDV _fn;
          struct dat_DDV _fo;
          unsigned int _fp;
          unsigned int _fq;
          struct dat_DDV _fr;
          arr_f5 = w_d;
          _f6 = iu19993_f1;
          _f4 = arr_f5.data[_f6];
          _f8 = iu20003_er;
          _f9 = _f4;
          _f7.index = (_f8 == _f9) ? 0 : 1;
          arr_fb = doc_e;
          _fc = iu19993_f1;
          _fa = arr_fb.data[_fc];
          arr_fe = z_c;
          _ff = _fa;
          _fd = arr_fe.data[_ff];
          _fh = i_eg;
          _fi = _fd;
          _fg.index = (_fh == _fi) ? 0 : 1;
          _fk = _fg;
          _fl = _f7;
          _fj.index = (!(_fl.index == _fk.index));
          _fn = _fj;
           if ((_fn.index == 0))
           {
             _fm = 1;
           }
           else
            if ((_fn.index == 1))
            {
              _fm = 0;
            }
          _fp = _fa;
          _fq = docUpdate_f;
          _fo.index = (_fp == _fq) ? 0 : 1;
          _fr = _fo;
           if ((_fr.index == 0))
           {
             _f3 = 0;
           }
           else
            if ((_fr.index == 1))
            {
              _f3 = _fm;
            }
          acc_f2 += _f3;
          _ey = acc_f2;
        }
        /* ----------- End Summate ----------- */
        _ft = _ey;
        p_fu = log1p((_ft - 1));
        _fs = p_fu;
        _fw = 0;
        _fx = _h;
        /* ---------- Begin Summate ---------- */
        acc_fz = 0;
        for (iu19993_fy = _fw; iu19993_fy < _fx; iu19993_fy++)
        {
          unsigned int _g0;
          unsigned int _g1;
          struct array_nat arr_g2;
          unsigned int _g3;
          struct dat_DDV _g4;
          unsigned int _g5;
          unsigned int _g6;
          struct dat_DDV _g7;
          struct dat_DDV _g8;
          struct dat_DDV _g9;
          unsigned int _ga;
          struct dat_DDV _gb;
          unsigned int _gc;
          struct array_nat arr_gd;
          unsigned int _ge;
          struct dat_DDV _gf;
          unsigned int _gg;
          unsigned int _gh;
          struct dat_DDV _gi;
          arr_g2 = w_d;
          _g3 = iu19993_fy;
          _g1 = arr_g2.data[_g3];
          _g5 = iu20003_er;
          _g6 = _g1;
          _g4.index = (_g5 == _g6) ? 0 : 1;
          _g8 = _ek;
          _g9 = _g4;
          _g7.index = (!(_g9.index == _g8.index));
          _gb = _g7;
           if ((_gb.index == 0))
           {
             _ga = 1;
           }
           else
            if ((_gb.index == 1))
            {
              _ga = 0;
            }
          arr_gd = doc_e;
          _ge = iu19993_fy;
          _gc = arr_gd.data[_ge];
          _gg = docUpdate_f;
          _gh = _gc;
          _gf.index = (_gg == _gh) ? 0 : 1;
          _gi = _gf;
           if ((_gi.index == 0))
           {
             _g0 = _ga;
           }
           else
            if ((_gi.index == 1))
            {
              _g0 = 0;
            }
          acc_fz += _g0;
          _fv = acc_fz;
        }
        /* ----------- End Summate ----------- */
        _gj = 0;
        _gk = _fv;
        /* ---------- Begin Product ---------- */
        t_gl = 0.0;
        c_gm = 0.0;
        for (j_gn = _gj; j_gn < _gk; j_gn++)
        {
          double x_go;
          double y_gp;
          double z_gq;
          double _gr;
          unsigned int _gs;
          double p_gt;
          double _gu;
          double _gv;
          double _gw;
          _gs = j_gn;
          p_gt = log1p((_gs - 1));
          _gr = p_gt;
          _gu = _fs;
          _gv = _gr;
          _gw = _ev;
          x_go = logSumExp3(_gu,_gv,_gw);
          y_gp = (x_go - c_gm);
          z_gq = (t_gl + y_gp);
          c_gm = ((z_gq - t_gl) - y_gp);
          t_gl = z_gq;
        }
        x_es = t_gl;
        /* ----------- End Product ----------- */
        y_et = (x_es - c_eq);
        z_eu = (t_ep + y_et);
        c_eq = ((z_eu - t_ep) - y_et);
        t_ep = z_eu;
      }
      x_eh = t_ep;
      /* ----------- End Product ----------- */
      y_ei = (x_eh - c_ef);
      z_ej = (t_ee + y_ei);
      c_ef = ((z_ej - t_ee) - y_ei);
      t_ee = z_ej;
    }
    _eb = t_ee;
    /* ----------- End Product ----------- */
    _gx = _eb;
    _gy = _e8;
    _gz = _az;
    _h0 = _di;
    _b1.data[zNewu19983_b2] = (_gy + (_gz + (_h0 + _gx)));
  }
  _h1 = _b1;
  ws_h3 = log(0);
  max_h4 = log(0.0);
  for (it_h2 = 0; it_h2 < _h1.size; it_h2++)
  {
    if ((max_h4 < _h1.data[it_h2]))
    {
      max_h4 = _h1.data[it_h2];
    }
  }
  for (it_h2 = 0; it_h2 < _h1.size; it_h2++)
  {
    ws_h3 = logSumExp2(ws_h3,(_h1.data[it_h2] - max_h4));
  }
  r_h5 = ((((double)rand()) / ((double)RAND_MAX)) * exp(ws_h3));
  ws_h3 = log(0);
  it_h2 = 0;
  while (1)
  {
    if ((r_h5 < exp(ws_h3)))
    {
      out_g.weight = 0;
      out_g.sample = (it_h2 - 1);
      break;
    }
    ws_h3 = logSumExp2(ws_h3,(_h1.data[it_h2] - max_h4));
    it_h2++;
  }
  return out_g;
}

