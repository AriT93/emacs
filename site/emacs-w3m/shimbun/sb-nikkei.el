;;; sb-nikkei.el --- shimbun backend for nikkei.co.jp -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007
;; Kazuyoshi KOREEDA <Koreeda.Kazuyoshi@jp.panasonic.com>

;; Author: Kazuyoshi KOREEDA <Koreeda.Kazuyoshi@jp.panasonic.com>,
;;         Katsumi Yamaoka   <yamaoka@jpl.org>,
;;         NOMIYA Masaru     <nomiya@ttmy.ne.jp>
;; Keywords: news

;; This file is a part of shimbun.

;; This program is free software; you can redistribute it a>nd/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; Original code was sb-asahi.el which is written by
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org> and
;; Yuuichi Teranishi <teranisi@gohome.org>.

;;; Code:

(require 'shimbun)

(luna-define-class shimbun-nikkei (shimbun-japanese-newspaper shimbun) ())

(defvar shimbun-nikkei-top-level-domain "nikkei.co.jp"
  "Name of the top level domain for the Nikkei Net.")

(defvar shimbun-nikkei-url
  (concat "http://www." shimbun-nikkei-top-level-domain "/")
  "Name of the parent url.")

(defvar shimbun-nikkei-group-table
  `(("top" "$B%H%C%W(B" ,shimbun-nikkei-url
     shimbun-nikkei-get-headers-top
     shimbun-nikkei-prepare-article-top)
    ("main" "$B<gMW(B" ,(concat shimbun-nikkei-url "news/main/")
     shimbun-nikkei-get-headers-default
     shimbun-nikkei-prepare-article-default)
    ("keizai" "$B7P:Q(B" ,(concat shimbun-nikkei-url "news/keizai/")
     shimbun-nikkei-get-headers-default
     shimbun-nikkei-prepare-article-default)
    ("sangyo" "$B4k6H(B" ,(concat shimbun-nikkei-url "news/sangyo/")
     shimbun-nikkei-get-headers-default
     shimbun-nikkei-prepare-article-default)
    ("tento" "$B%Y%s%A%c!<(B" ,(concat shimbun-nikkei-url "news/tento/")
     shimbun-nikkei-get-headers-default2
     shimbun-nikkei-prepare-article-default2)
    ("kansai" "$B4X@>(B" ,(concat shimbun-nikkei-url "kansai/")
     shimbun-nikkei-get-headers-kansai
     shimbun-nikkei-prepare-article-kansai)
    ("it.business" "IT$B%S%8%M%9(B"
     "http://it.nikkei.co.jp/business/news/index.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.busi_gyoukai" "$B6H3&F08~(B(IT$B%S%8%M%9(B)"
     "http://it.nikkei.co.jp/business/news/busi_gyoukai.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.biz-system" "$B4k6H>pJs%7%9%F%`(B"
     "http://it.nikkei.co.jp/business/news/busi_system.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.sox" "$B@9$j>e$,$k#S#O#XK!%S%8%M%9(B"
     "http://it.nikkei.co.jp/business/special/sox.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.data" "$B%G!<%?$GFI$`#I#T;T>l(B"
     "http://it.nikkei.co.jp/business/column/data.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.taidan" "$B%H%C%WBPCL(B"
     "http://it.nikkei.co.jp/business/column/taidan.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.internet" "$B%$%s%?!<%M%C%H(B"
     "http://it.nikkei.co.jp/internet/news/index.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.broad" "$B%V%m!<%I%P%s%I(B"
     "http://it.nikkei.co.jp/internet/news/broadband.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.net_gyoukai" "$B6H3&F08~(B($B%$%s%?!<%M%C%H(B)"
     "http://it.nikkei.co.jp/internet/news/net_gyoukai.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.iptel" "$BB?5!G=2=$9$k#I#PEEOC(B"
     "http://it.nikkei.co.jp/internet/special/iptel.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.tele" "$BJ|Aw!&%M%C%HM;9g(B"
     "http://it.nikkei.co.jp/internet/special/tele.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.broadcast" "$BCO>e%G%8%?%kJ|Aw(B"
     "http://it.nikkei.co.jp/internet/special/d_broadcast.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.internet-column" "$B%$%s%?!<%M%C%H(B:$B%3%i%`(B"
     "http://it.nikkei.co.jp/internet/column/koike.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.contents" "$B%3%s%F%s%D%S%8%M%9(B"
     "http://it.nikkei.co.jp/internet/column/contents.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.ec" "EC"
     "http://it.nikkei.co.jp/internet/news/ec.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.policy" "$B@/:v!&E}7W(B"
     "http://it.nikkei.co.jp/internet/news/seisaku.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.e-gov" "$B9T@/$N#I#T2=(B"
     "http://it.nikkei.co.jp/business/special/e-gov.aspx"
      shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.mobile" "$B%b%P%$%k(B"
     "http://it.nikkei.co.jp/mobile/news/index.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.mob_gyoukai" "$B6H3&F08~(B($B%b%P%$%k(B)"
     "http://it.nikkei.co.jp/mobile/news/gyoukai.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.mobsoft" "$B%5!<%S%9(B"
     "http://it.nikkei.co.jp/mobile/news/soft.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.mobcon" "$B%3%s%F%s%D(B"
     "http://it.nikkei.co.jp/mobile/news/contents.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.money" "$B7HBS%-%c%j%"$N6bM;%S%8%M%9(B"
     "http://it.nikkei.co.jp/mobile/special/money.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.one" "$B%o%s%;%0$O%F%l%S$rJQ$($k$+(B"
     "http://it.nikkei.co.jp/mobile/special/one.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.security" "$B%;%-%e%j%F%#(B"
     "http://it.nikkei.co.jp/security/news/index.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.net_crime" "$B%M%C%HHH:a(B"
     "http://it.nikkei.co.jp/security/news/net_crime.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.digital" "$B%G%8%?%k2HEE!u%(%s%?%a(B"
     "http://it.nikkei.co.jp/digital/news/index.aspx"
     shimbun-nikkei-get-headers-it-default
     shimbun-nikkei-prepare-article-default)
    ("it.pc" "PC$B!u%G%8%?%k%+%a%i(B"
     "http://it.nikkei.co.jp/pc/news/index.aspx?ichiran=True"
     shimbun-nikkei-get-headers-it-pc
     shimbun-nikkei-prepare-article-default2)
    ("kokunai" "$B;T>l3567(B" "http://markets.nikkei.co.jp/kokunai/summary.cfm"
     shimbun-nikkei-get-headers-kawase
     shimbun-nikkei-prepare-article-default3)
    ("markets" "$B3$303t3567(B" "http://markets.nikkei.co.jp/kaigai/summary.cfm"
     shimbun-nikkei-get-headers-markets
     shimbun-nikkei-prepare-article-default3)
    ("kawase" "$B0YBX3567(B" "http://markets.nikkei.co.jp/kawase/summary.cfm"
     shimbun-nikkei-get-headers-kawase
     shimbun-nikkei-prepare-article-default3)
    ("kinri" "$BC;4|6bMx!&:D8"!&#C#B3567(B"
     "http://markets.nikkei.co.jp/kawase/kinri.cfm"
     shimbun-nikkei-get-headers-kinri
     shimbun-nikkei-prepare-article-default3)
    ("ft" "$B1Q%U%#%J%s%7%c%k!&%?%$%`%:(B"
     "http://markets.nikkei.co.jp/kaigai/ft.cfm"
     shimbun-nikkei-get-headers-ft
     shimbun-nikkei-prepare-article-default3)
    ("dj" "$BJF%@%&!&%8%g!<%s%:(B" "http://markets.nikkei.co.jp/kaigai/dj.cfm"
     shimbun-nikkei-get-headers-dj
     shimbun-nikkei-prepare-article-default3)
    ("ngyoseki" "$B4k6H6H@S%K%e!<%9(B"
     "http://markets.nikkei.co.jp/kokunai/gyoseki.cfm"
     shimbun-nikkei-get-headers-gyoseki
     shimbun-nikkei-prepare-article-default3)
    ("gyosuuchi" "$B6H@S?tCM(B"
     "http://markets.nikkei.co.jp/kokunai/bunkatsu3.cfm?genre=m4"
     shimbun-nikkei-get-headers-bunkatsu2
     shimbun-nikkei-prepare-article-bunkatsu2)
    ("gyoseki" "$B3$304k6H6H@S(B" "http://markets.nikkei.co.jp/kaigai/gyoseki.cfm"
     shimbun-nikkei-get-headers-gyoseki
     shimbun-nikkei-prepare-article-default3)
    ("china" "$BCf9q%S%8%M%9;v>p(B" ,(concat shimbun-nikkei-url "china/news/")
     shimbun-nikkei-get-headers-china
     shimbun-nikkei-prepare-article-default4)
    ("market" "$B3t!&0YBX(B" ,(concat shimbun-nikkei-url "news/market/")
     shimbun-nikkei-get-headers-market
     shimbun-nikkei-prepare-article-market)
    ("kaigai" "$B9q:](B" ,(concat shimbun-nikkei-url "news/kaigai/")
     shimbun-nikkei-get-headers-default
     shimbun-nikkei-prepare-article-default)
    ("seiji" "$B@/<#(B" ,(concat shimbun-nikkei-url "news/seiji/")
     shimbun-nikkei-get-headers-default
     shimbun-nikkei-prepare-article-default)
    ("shakai" "$B<R2q(B" ,(concat shimbun-nikkei-url "news/shakai/")
     shimbun-nikkei-get-headers-default
     shimbun-nikkei-prepare-article-default)
    ("retto" "$BCO0h7P:Q(B" ,(concat shimbun-nikkei-url "news/retto/")
     shimbun-nikkei-get-headers-retto
     shimbun-nikkei-prepare-article-default4)
    ("sports" "$B%9%]!<%D(B" "http://sports.nikkei.co.jp/"
     shimbun-nikkei-get-headers-sports
     shimbun-nikkei-prepare-article-sports)
    ("newpro" "$B?7@=IJ(B" ,(concat shimbun-nikkei-url "newpro/news/")
     shimbun-nikkei-get-headers-newpro
     shimbun-nikkei-prepare-article-newpro)
    ("release" "$B%W%l%9%j%j!<%9(B" "http://release.nikkei.co.jp/"
     shimbun-nikkei-get-headers-release
     shimbun-nikkei-prepare-article-release)
    ("release.it.comp" "$B%W%l%9%j%j!<%9(B($B#I#T!(%3%s%T%e!<%?!<(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=1&sindID=1"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.it.peri" "$B%W%l%9%j%j!<%9(B($B#I#T!(<~JU5!4o(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=1&sindID=2"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.it.sys" "$B%W%l%9%j%j!<%9(B($B#I#T!(%7%9%F%`!&%=%U%H3+H/(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=1&sindID=3"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.it.cont" "$B%W%l%9%j%j!<%9(B($B#I#T!(>pJs!&%3%s%F%s%D(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=1&sindID=4"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.it.net" "$B%W%l%9%j%j!<%9(B($B#I#T!(DL?.!&%$%s%?!<%M%C%H(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=1&sindID=5"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.it.lsi" "$B%W%l%9%j%j!<%9(B($B#I#T!(H>F3BN(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=1&sindID=6"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.it.game" "$B%W%l%9%j%j!<%9(B($B#I#T!(%2!<%`!&8d3Z(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=1&sindID=7"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.it.etc" "$B%W%l%9%j%j!<%9(B($B#I#T!($=$NB>#I#T4XO"(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=1&sindID=8"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.dist.depart" "$B%W%l%9%j%j!<%9(B($BN.DL!(I42_E9!&%9!<%Q!<(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=2&sindID=9"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.dist.ryohan" "$B%W%l%9%j%j!<%9(B($BN.DL!(NLHNE9(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=2&sindID=10"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.dist.zakka" "$B%W%l%9%j%j!<%9(B($BN.DL!(@83h;(2_(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=2&sindID=11"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.dist.cosme" "$B%W%l%9%j%j!<%9(B($BN.DL!(0eLtIJ!&2=>QIJ(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=2&sindID=12"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.dist.car" "$B%W%l%9%j%j!<%9(B($BN.DL!(<+F0<V(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=2&sindID=13"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.dist.book" "$B%W%l%9%j%j!<%9(B($BN.DL!(=q@R(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=2&sindID=14"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.dist.record" "$B%W%l%9%j%j!<%9(B($BN.DL!(%l%3!<%I!&%2!<%`(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=2&sindID=15"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.dist.food" "$B%W%l%9%j%j!<%9(B($BN.DL!(?)IJ!&0{NA(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=2&sindID=16"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.dist.mercha" "$B%W%l%9%j%j!<%9(B($BN.DL!(>&<R!&27Gd6H(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=2&sindID=17"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.dist.mail" "$B%W%l%9%j%j!<%9(B($BN.DL!(DL?.!&K,LdHNGd(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=2&sindID=18"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.dist.netshop" "$B%W%l%9%j%j!<%9(B($BN.DL!(%M%C%H%7%g%C%T%s%0(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=2&sindID=19"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.dist.etc" "$B%W%l%9%j%j!<%9(B($BN.DL!($=$NB>>&6H(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=2&sindID=20"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.money.bank" "$B%W%l%9%j%j!<%9(B($B6bM;!(6d9T!&?.6b(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=3&sindID=57"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.money.sec" "$B%W%l%9%j%j!<%9(B($B6bM;!(>Z7t2q<R(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=3&sindID=58"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.money.am" "$B%W%l%9%j%j!<%9(B($B6bM;!(Ej;q?.Bw1?MQ2q<R(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=3&sindID=59"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.money.insu" "$B%W%l%9%j%j!<%9(B($B6bM;!(J]812q<R(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=3&sindID=60"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.money.etc" "$B%W%l%9%j%j!<%9(B($B6bM;!($=$NB>6bM;(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=3&sindID=61"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.maker.chemi" "$B%W%l%9%j%j!<%9(B($B%a!<%+!<!(2=3X!&0eLtIJ(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=4&sindID=31"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.maker.mecha" "$B%W%l%9%j%j!<%9(B($B%a!<%+!<!(5!3#!&6bB0(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=4&sindID=32"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.maker.car" "$B%W%l%9%j%j!<%9(B($B%a!<%+!<!(<+F0<V!&<+F0<VItIJ(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=4&sindID=33"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.maker.elec" "$B%W%l%9%j%j!<%9(B($B%a!<%+!<!(2HEE!&EE5!(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=4&sindID=34"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.maker.food" "$B%W%l%9%j%j!<%9(B($B%a!<%+!<!(?)IJ!&0{NA(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=4&sindID=35"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.maker.sports" "$B%W%l%9%j%j!<%9(B($B%a!<%+!<!(%9%]!<%D!&8d3ZMQIJ(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=4&sindID=36"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.maker.apparel" "$B%W%l%9%j%j!<%9(B($B%a!<%+!<!(%"%Q%l%k!&@83hMQIJ(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=4&sindID=37"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.maker.commu" "$B%W%l%9%j%j!<%9(B($B%a!<%+!<!(DL?.5!4o!&@:L)5!3#(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=4&sindID=38"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.maker.etc" "$B%W%l%9%j%j!<%9(B($B%a!<%+!<!($=$NB>%a!<%+!<(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=4&sindID=39"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.service.medic" "$B%W%l%9%j%j!<%9(B($B%5!<%S%9!(0eNE!&J!;c(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=5&sindID=40"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.service.rest" "$B%W%l%9%j%j!<%9(B($B%5!<%S%9!(0{?)(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=5&sindID=41"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.service.trans" "$B%W%l%9%j%j!<%9(B($B%5!<%S%9!(1?M"!&1?Aw(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=5&sindID=42"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.service.energy" "$B%W%l%9%j%j!<%9(B($B%5!<%S%9!(%(%M%k%.!<(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=5&sindID=43"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.service.enter" "$B%W%l%9%j%j!<%9(B($B%5!<%S%9!(%(%s%?!<%F%$%s%a%s%H(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=5&sindID=44"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.service.env" "$B%W%l%9%j%j!<%9(B($B%5!<%S%9!(4D6-(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=5&sindID=45"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.service.consul" "$B%W%l%9%j%j!<%9(B($B%5!<%S%9!(%3%s%5%k%F%#%s%0(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=5&sindID=46"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.service.edu" "$B%W%l%9%j%j!<%9(B($B%5!<%S%9!(650i!&8&=$(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=5&sindID=47"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.service.haken" "$B%W%l%9%j%j!<%9(B($B%5!<%S%9!(?M:`GI8/(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=5&sindID=48"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.service.life" "$B%W%l%9%j%j!<%9(B($B%5!<%S%9!(@83h4XO"(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=5&sindID=49"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.service.media" "$B%W%l%9%j%j!<%9(B($B%5!<%S%9!(%a%G%#%"(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=5&sindID=50"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.service.lease" "$B%W%l%9%j%j!<%9(B($B%5!<%S%9!(%j!<%9(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=5&sindID=51"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.service.travel" "$B%W%l%9%j%j!<%9(B($B%5!<%S%9!(N99T!&%[%F%k(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=5&sindID=52"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.service.etc" "$B%W%l%9%j%j!<%9(B($B%5!<%S%9!($=$NB>%5!<%S%96H(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=5&sindID=53"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.const.const" "$B%W%l%9%j%j!<%9(B($B%5!<%S%9!(7z@_!&EZLZ(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=6&sindID=54"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.const.house" "$B%W%l%9%j%j!<%9(B($B%5!<%S%9!(=;Bp(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=6&sindID=56"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.const.etc" "$B%W%l%9%j%j!<%9(B($B%5!<%S%9!($=$NB>7z@_4XO"(B)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=5&sindID=53"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("shasetsu" "$B<R@b!&=U=)(B"
     "http://www.nikkei.co.jp/news/shasetsu/childKijiIchiran.js"
     ;; The contents of IndexKijiIchiran.js will be appended afterward.
     shimbun-nikkei-get-headers-shasetsu
     shimbun-nikkei-prepare-article-default))
  "Alist of group names and parameters.
Each parameters include a Japanese group name, an index page, a
function used to get headers and a function used to prepare an article.")

(defvar shimbun-nikkei-server-name "$BF|K\7P:Q?7J9(B")
(defvar shimbun-nikkei-from-address "nobody@example.com")
(defvar shimbun-nikkei-content-start
  "<!--emacs-w3m-shimbun-nikkei-content-start-->")
(defvar shimbun-nikkei-content-end
  "<!--emacs-w3m-shimbun-nikkei-content-end-->")
(defvar shimbun-nikkei-x-face-alist
  '(("default" . "\
Face: iVBORw0KGgoAAAANSUhEUgAAADAAAAAWAgMAAAD7mfc/AAAABGdBTUEAALGPC/xhBQAAAAx
 QTFRFBjKeZ4rcxdHp+/z7lhoK9wAAAC90RVh0U29mdHdhcmUAWFYgVmVyc2lvbiAzLjEwYStGTG1
 hc2sgIFJldjogMTIvMjkvOTQbx6p8AAABBUlEQVR4nD3MIU/DQBwF8D8d62W9wGqalYRSREUV9xX
 aC0snKzgQC8GgmUFg2Lh+AhD11dUYEsj2Ea6i/swEbqKiTZYeHIJnXn7iPehwXRUfs7tpbL5Biyo
 uZOiZYHJoUMb5y3twhhSFnZHR2NjZz7hHIGkyiJFMP8FCILhpIiqKp+EthkqMLMQ3TUD2Y8jEyQi
 LuLFJ6wMVrjsWRgvOPYFEBqEvjSuAcwJK55uVuv7Qs7n6xzYlSWrYoNHn6YV1cJ06Gh2LvCOs5Eo
 jZ9Gph5VYg57fTN0Q/I1Gx+bDw9BZcP22ZQ8WPBKVadTs6xiKlaIaOdv70SssB7/oy7JbxPXlcqJ
 +AFOYhEr5ENrbAAAAB3RJTUUH1AQGFzot7I86fAAAAABJRU5ErkJggg==")
    ("\\`release" . "\
Face: iVBORw0KGgoAAAANSUhEUgAAADAAAAATBAMAAAAkFJMsAAAABGdBTUEAALGPC/xhBQAAABJ
 QTFRFAAAAZI9jnMGYtNiv1+3U////kl1YDAAAAC90RVh0U29mdHdhcmUAWFYgVmVyc2lvbiAzLjE
 wYStGTG1hc2sgIFJldjogMTIvMjkvOTQbx6p8AAABM0lEQVR4nDVRQZKEIAwMg97DoHddnTsI3NG
 BOwj5/1c2WzvTKZJ0NZWiA/hVCsewf5mPAUAAEOAOoVFrZ7V6OrdZ+IcANVFRtNEP2UChP3LNRHW
 ugDPVB000cJzhngnoVJgLqEBtanMbe26Qb4oP4klkQJHvcwllblQEdVpGSkmzKEhSiDnmMveJkLi
 4Y+wAsgtijmRGmknkxuNhrAJelQXJAZKoPQLRfWAoAvYI/hwu+QZIKWqfGIvH578d9bW1HZ/uyf7
 F+pcQEBfl3MrXmD7h1fXBnVqEzNemmxWoEEHDPOxpX/3xxsFUb6sz0mqrN3jl4eq1uYZDKr3WkPS
 VbK8QYMBqXNxxWMpti1fTpettwLPgTcIEEs/dxClO5xQ970ogfMGv+TL+D/UVhFBKreuH/QJ8OEG
 oiorzBAAAAAd0SU1FB9QECQYoFI75G7YAAABodEVYdENvbW1lbnQAQ1JFQVRPUjogWFYgVmVyc2l
 vbiAzLjEwYStGTG1hc2sgIFJldjogMTIvMjkvOTQKQ1JFQVRPUjogWFYgVmVyc2lvbiAzLjEwYSt
 GTG1hc2sgIFJldjogMTIvMjkvOTQKYD1XBQAAAABJRU5ErkJggg==")))

(defvar shimbun-nikkei-expiration-days 7)

(luna-define-method shimbun-groups ((shimbun shimbun-nikkei))
  (mapcar 'car shimbun-nikkei-group-table))

(luna-define-method shimbun-current-group-name ((shimbun shimbun-nikkei))
  (nth 1 (assoc (shimbun-current-group-internal shimbun)
		shimbun-nikkei-group-table)))

(luna-define-method shimbun-index-url ((shimbun shimbun-nikkei))
  (nth 2 (assoc (shimbun-current-group-internal shimbun)
		shimbun-nikkei-group-table)))

(luna-define-method shimbun-get-headers ((shimbun shimbun-nikkei)
					 &optional range)
  (let* ((case-fold-search t)
	 (group (shimbun-current-group-internal shimbun))
	 (fn (nth 3 (assoc group shimbun-nikkei-group-table)))
	 (shimbun-nikkei-from-address
	  (concat (shimbun-server-name shimbun)
		  " (" (shimbun-current-group-name shimbun) ")"))
	 (folder (nth 2 (assoc group shimbun-nikkei-group-table))))
    (when (and (not (string-match "\\`http://markets\\.nikkei\\.co\\.jp/"
				  folder))
	       (or (member group '("kaigai" "seiji"))
		   (not (eq (aref folder (1- (length folder))) ?/)))
	       (string-match "[^/]/[^/]\\|[^/]/\\'" folder))
      (setq folder (substring folder 0 (+ (match-beginning 0) 2))))
    (while (search-forward "\r" nil t)
      (delete-backward-char 1))
    (goto-char (point-min))
    (when (fboundp fn)
      (shimbun-sort-headers (funcall fn group folder shimbun range)))))

(defun shimbun-nikkei-expand-url (url folder)
  "Make a fullname of URL relative to FOLDER.
If URL begins with `http://', FOLDER is ignored."
  (save-match-data
    (cond ((string-match "=http://" url)
	   (substring url (1+ (match-beginning 0))))
	  ((string-match "\\`http://" url)
	   url)
	  ((string-match "\\`/" url)
	   (concat folder (substring url 1)))
	  (t
	   (concat folder url)))))

(defun shimbun-nikkei-make-date-string (&rest args)
  "Run `shimbun-make-date-string' with ARGS and fix a day if needed.

\(shimbun-nikkei-make-date-string YEAR MONTH DAY &optional TIME TIMEZONE)"
  (save-match-data
    (let* ((ctime (current-time))
	   (date (apply 'shimbun-make-date-string args))
	   (time (shimbun-time-parse-string date))
	   (ms (car time))
	   (ls (cadr time))
	   (system-time-locale "C"))
      (if (or (> ms (car ctime))
	      (and (= ms (car ctime))
		   (> ls (cadr ctime))))
	  ;; It should be yesterday's same time.
	  (progn
	    (setq ms (1- ms))
	    (when (< (setq ls (- ls (eval-when-compile
				      (- (* 60 60 24) 65536))))
		     0)
	      (setq ms (1- ms)
		    ls (+ ls 65536)))
	    (format-time-string "%a, %d %b %Y %R +0900" (list ms ls)))
	date))))

(defvar shimbun-nikkei-tmp-ids nil)
(defvar shimbun-nikkei-tmp-subjects nil)

(defun shimbun-nikkei-get-headers-default (group folder shimbun range
						 &optional headers)
  "Default function used to fetch headers.
GROUP is a group name.  FOLDER is a parent url.
If HEADERS is non-nil, it is appended to newly fetched headers."
  (unless headers
    (setq shimbun-nikkei-tmp-ids nil
	  shimbun-nikkei-tmp-subjects nil))
  (let (id subject time)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\"/?"
			;; 1. url
			"\\(\\(?:[^\">]+/\\)?"
			"\\(?:\\(?:20[0-9][0-9][01][0-9][0-3][0-9]"
			;; 2. serial number
			"\\([0-9a-z]+"
			;; 3. day
			"\\([0-3][0-9]\\)"
			;; 4. month
			"\\([01][0-9]\\)"
			;; 5. year
			"\\(20[0-9][0-9]\\)"
			"\\)\\)\\|\\(?:"
			;; 6. serial number
			"\\("
			;; 7. year
			"\\(20[0-9][0-9]\\)"
			;; 8. month
			"\\([01][0-9]\\)"
			;; 9. day
			"\\([0-3][0-9]\\)"
			"[0-9a-z]+\\)\\)\\)"
			"\\.html\\)"
			s0 "\"[^>]*>\\(?:" s0 "<[^>]+>" s0 "\\)*" s0
			;; 10. subject
			"\\([^<]*\\(?:\\(?:<[^>]+>\\)?[^<]+\\)+"
			"\\(?:<[^>]+>\\)?\\)"
			s0 "</a>")))
	    nil t)
      (unless (or (member (setq id (concat
				    "<" (downcase (if (match-beginning 2)
						      (concat (match-string 5)
							      (match-string 4)
							      (match-string 3)
							      (match-string 2))
						    (match-string 6)))
				    "%" group "."
				    shimbun-nikkei-top-level-domain
				    ">"))
			  shimbun-nikkei-tmp-ids)
		  (progn
		    (save-match-data
		      (if (string-match
			   " ?(\\([012][0-9]:[0-5][0-9]\\)) ?\\'"
			   (setq subject (shimbun-replace-in-string
					  (match-string 10)
					  "[\t\n $B!!(B]+" " ")))
			  (setq time (match-string 1 subject)
				subject (substring subject 0
						   (match-beginning 0)))
			(setq time nil)))
		    (and (>= (length subject) 16)
			 (member subject shimbun-nikkei-tmp-subjects))))
	(push id shimbun-nikkei-tmp-ids)
	(push subject shimbun-nikkei-tmp-subjects)
	(push (shimbun-create-header
	       0 subject shimbun-nikkei-from-address
	       (shimbun-nikkei-make-date-string
		(string-to-number (or (match-string 5) (match-string 7)))
		(string-to-number (or (match-string 4) (match-string 8)))
		(string-to-number (or (match-string 3) (match-string 9)))
		time)
	       id "" 0 0
	       (shimbun-nikkei-expand-url (match-string 1) folder))
	      headers)))
    headers))

(defun shimbun-nikkei-get-headers-top (group folder shimbun range)
  "Function used to fetch headers for the `top' group."
  (let (id subject headers time)
    (setq shimbun-nikkei-tmp-ids nil)
    (setq shimbun-nikkei-tmp-subjects nil)
    (when (re-search-forward
	   (eval-when-compile
	     (let ((s0 "[\t\n ]*")
		   (s1 "[\t\n ]+"))
	       (concat
		"<a" s1 "href=\"/?"
		;; 1. url
		"\\([^\"]+/"
		"\\(?:\\(?:20[0-9][0-9][01][0-9][0-3][0-9]"
		;; 2. serial number
		"\\([0-9a-z]+"
		;; 3. day
		"\\([0-3][0-9]\\)"
		;; 4. month
		"\\([01][0-9]\\)"
		;; 5. year
		"\\(20[0-9][0-9]\\)"
		"\\)\\)\\|\\(?:"
		;; 6. serial number
		"\\("
		;; 7. year
		"\\(20[0-9][0-9]\\)"
		;; 8. month
		"\\([01][0-9]\\)"
		;; 9. day
		"\\([0-3][0-9]\\)"
		"[0-9a-z]+\\)\\)\\)"
		"\\.html\\)"
		".+>" s0
		"<!-+" s0 "FJZONE" s1 "START" s1 "NAME=\"MIDASHI\""
		s0 "-+>" s0
		;; 10. subject
		"\\([^<]+[^\t\n ]\\)"
		s0
		"<!-+" s0 "FJZONE" s1 "END" s1 "NAME=\"MIDASHI\""
		s0 "-+>\\(?:" s0 "<[^!]+>\\)*" s0
		"<!-+" s0 "FJZONE" s1 "START" s1 "NAME=\"HONBUN\""
		s0 "-+>[^<]+("
		;; 11. hour:minute
		"\\([0-2][0-9]:[0-5][0-9]\\)")))
	   nil t)
      (unless (or (member (setq id (concat
				    "<" (downcase (if (match-beginning 2)
						      (concat (match-string 5)
							      (match-string 4)
							      (match-string 3)
							      (match-string 2))
						    (match-string 6)))
				    "%" group "."
				    shimbun-nikkei-top-level-domain
				    ">"))
			  shimbun-nikkei-tmp-ids)
		  (and (>= (length (setq subject (match-string 10))) 16)
		       (member subject shimbun-nikkei-tmp-subjects)))
	(push id shimbun-nikkei-tmp-ids)
	(push subject shimbun-nikkei-tmp-subjects)
	(push (shimbun-create-header
	       0 subject shimbun-nikkei-from-address
	       (shimbun-nikkei-make-date-string
		(string-to-number (or (match-string 5) (match-string 7)))
		(string-to-number (or (match-string 4) (match-string 8)))
		(string-to-number (or (match-string 3) (match-string 9)))
		(match-string 11))
	       id "" 0 0
	       (shimbun-nikkei-expand-url (match-string 1) folder))
	      headers)))

    (setq headers (shimbun-nikkei-get-headers-default
		   group folder shimbun range headers))

    (goto-char (point-min))
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n $B!!(B]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\"/?"
			;; 1. url
			"\\(\\(?:[^\"/]+/\\)+"
			"\\(?:\\(?:20[0-9][0-9][01][0-9][0-3][0-9]"
			;; 2. serial number
			"\\([0-9a-z]+"
			;; 3. day
			"\\([0-3][0-9]\\)"
			;; 4. month
			"\\([01][0-9]\\)"
			;; 5. year
			"\\(20[0-9][0-9]\\)"
			"\\)\\)\\|\\(?:"
			;; 6. serial number
			"\\("
			;; 7. year
			"\\(20[0-9][0-9]\\)"
			;; 8. month
			"\\([01][0-9]\\)"
			;; 9. day
			"\\([0-3][0-9]\\)"
			"[0-9a-z]+\\)\\)\\)"
			"\\.html\\)"
			s0 "\"[^>]*>" s0
			;; 10. subject
			"\\([^<]+\\)"
			s0 "</a>")))
	    nil t)
      (setq subject (match-string 10))
      (unless (or (member (setq id (concat
				    "<" (shimbun-subst-char-in-string
					 ?  ?_
					 (downcase
					  (if (match-beginning 2)
					      (concat (match-string 5)
						      (match-string 4)
						      (match-string 3)
						      (match-string 2))
					    (match-string 6))))
				    "%" group "."
				    shimbun-nikkei-top-level-domain
				    ">"))
			  shimbun-nikkei-tmp-ids)
		  (save-match-data
		    (string-match "&gt;&gt;&nbsp;$BB3$-(B" subject))
		  (progn
		    (save-match-data
		      (if (string-match
			   " ?(\\([012][0-9]:[0-5][0-9]\\)) ?\\'"
			   (setq subject (shimbun-replace-in-string
					  subject "[\t\n $B!!(B]+" " ")))
			  (setq time (match-string 1 subject)
				subject (substring subject 0
						   (match-beginning 0)))
			(setq time nil)))
		    (and (>= (length subject) 16)
			 (member subject shimbun-nikkei-tmp-subjects))))
	(push id shimbun-nikkei-tmp-ids)
	(push subject shimbun-nikkei-tmp-subjects)
	(push (shimbun-create-header
	       0 subject shimbun-nikkei-from-address
	       (shimbun-nikkei-make-date-string
		(string-to-number (or (match-string 5) (match-string 7)))
		(string-to-number (or (match-string 4) (match-string 8)))
		(string-to-number (or (match-string 3) (match-string 9)))
		time)
	       id "" 0 0
	       (shimbun-nikkei-expand-url (match-string 1) folder))
	      headers)))

    (goto-char (point-min))
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n $B!!(B]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\"/?"
			;; 1. url
			"\\([^\"]+/[^\"/]+="
			;; 2. serial number
			"\\([^\"/]+"
			;; 3. day
			"\\([0-3][0-9]\\)"
			;; 4. month
			"\\([01][0-9]\\)"
			;; 5. year
			"\\(20[0-9][0-9]\\)"
			"\\)\\)"
			s0 "\"[^>]*>" s0
			;; 6. subject
			"\\([^<]+\\)"
			s0 "</a>")))
	    nil t)
      (unless (or (member (setq id (concat "<" (shimbun-subst-char-in-string
						?  ?_
						(downcase (match-string 2)))
					   "%" group "."
					   shimbun-nikkei-top-level-domain
					   ">"))
			  shimbun-nikkei-tmp-ids)
		  (progn
		    (save-match-data
		      (if (string-match "\
\[\t\n $B!!(B]*(\\([012][0-9]:[0-5][0-9]\\))[\t\n $B!!(B]*\\'"
					(setq subject (match-string 6)))
			  (setq time (match-string 1 subject)
				subject (substring subject 0
						   (match-beginning 0)))
			(setq time nil)))
		    (and (>= (length subject) 16)
			 (member subject shimbun-nikkei-tmp-subjects))))
	(push id shimbun-nikkei-tmp-ids)
	(push subject shimbun-nikkei-tmp-subjects)
	(push (shimbun-create-header
	       0 subject shimbun-nikkei-from-address
	       (shimbun-nikkei-make-date-string
		(string-to-number (match-string 5))
		(string-to-number (match-string 4))
		(string-to-number (match-string 3))
		time)
	       id "" 0 0
	       (shimbun-nikkei-expand-url (match-string 1) folder))
	      headers)))

    (goto-char (point-min))
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n $B!!(B]*")
		    (s1 "[\t\n ]+"))
		(concat
		 "<a" s1 "href=\"/?"
		 ;; 1. url
		 "\\([^\"]+/[^\"/]+="
		 ;; 2. serial number
		 "\\("
		 ;; 3. year
		 "\\(20[0-9][0-9]\\)"
		 ;; 4. month
		 "\\([01][0-9]\\)"
		 ;; 5. day
		 "\\([0-3][0-9]\\)"
		 "[^\t\n \"&]+\\)"
		 "\\(?:&[^\t\n \">]+\\)?\\)"
		 s0 "\"[^>]*>" s0
		 ;; 6. subject
		 "\\([^<]*\\(?:\\(?:<br>\\)?[^<]+\\)+\\(?:<br>\\)?\\)"
		 s0 "</a>")))
	    nil t)
      (unless (or (member (setq id (concat "<" (downcase (match-string 2))
					   "%" group "."
					   shimbun-nikkei-top-level-domain
					   ">"))
			  shimbun-nikkei-tmp-ids)
		  (progn
		    (save-match-data
		      (setq subject (shimbun-replace-in-string
				     (match-string 6)
				     "[\t\n ]*<br>[\t\n ]*" " "))
		      (if (string-match "\
\[\t\n $B!!(B]*(\\([012][0-9]:[0-5][0-9]\\))[\t\n $B!!(B]*\\'"
					subject)
			  (setq time (match-string 1 subject)
				subject (substring subject 0
						   (match-beginning 0)))
			(setq time nil)))
		    (and (>= (length subject) 16)
			 (member subject shimbun-nikkei-tmp-subjects))))
	(push id shimbun-nikkei-tmp-ids)
	(push subject shimbun-nikkei-tmp-subjects)
	(push (shimbun-create-header
	       0 subject shimbun-nikkei-from-address
	       (shimbun-nikkei-make-date-string
		(string-to-number (match-string 3))
		(string-to-number (match-string 4))
		(string-to-number (match-string 5))
		time)
	       id "" 0 0
	       (shimbun-nikkei-expand-url (match-string 1) folder))
	      headers)))

    headers))

(defun shimbun-nikkei-get-headers-default2 (group folder shimbun range)
  "Function used to fetch headers for the tento and the zinzi groups."
  (let (headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\""
			;; 1. url
			"\\(\\(?:[^\"<>]+/\\)?"
			;; 2. serial number
			"\\("
			;; 3. year
			"\\(20[0-9][0-9]\\)"
			;; 4. month
			"\\([01][0-9]\\)"
			;; 5. day
			"\\([0-3][0-9]\\)"
			"[0-9a-z]+\\)"
			"\\.html\\)"
			s0 "\"" s0 ">" s0
			;; 6. subject
			"\\([^<>]+\\)")))
	    nil t)
      (push (shimbun-create-header
	     0
	     (match-string 6)
	     shimbun-nikkei-from-address
	     (shimbun-nikkei-make-date-string
	      (string-to-number (match-string 3))
	      (string-to-number (match-string 4))
	      (string-to-number (match-string 5)))
	     (concat "<" (downcase (match-string 2)) "%" group "."
		     shimbun-nikkei-top-level-domain ">")
	     "" 0 0
	     (shimbun-nikkei-expand-url (match-string 1) folder))
	    headers))
    headers))

(defun shimbun-nikkei-get-headers-kansai (group folder shimbun range)
  "Function used to fetch headers for the kansai group."
  (let ((date (if (re-search-forward
		   (eval-when-compile
		     (let ((s0 "[\t\n ]*")
			   (s1 "[\t\n ]+"))
		       (concat "class=\"date\"><strong>" s0
			       ;; 1. year
			       "\\(20[0-9][0-9]\\)"
			       "$BG/(B"
			       ;; 2. month
			       "\\([01]?[0-9]\\)"
			       "$B7n(B"
			       ;; 3. day
			       "\\([0-3]?[0-9]\\)"
			       "$BF|(B" s0 "([^<]+)"
			       s0 "</strong></td>")))
		   nil t)
		  (prog1
		      (shimbun-make-date-string
		       (string-to-number (match-string 1))
		       (string-to-number (match-string 2))
		       (string-to-number (match-string 3)))
		    (goto-char (point-min)))
		(let ((cts (current-time-string)))
		  (format "%s, %02d %s %s 00:00 +0900"
			  (substring cts 0 3)
			  (string-to-number (substring cts 8 10))
			  (substring cts 4 7)
			  (substring cts 20)))))
	headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\"\\./"
			;; 1. url
			"\\([^\"<>]+/"
			;; 2. serial number
			"\\([^\t\n ]+\\)"
			"\\)"
			s0 "-frame" s0 "\\.html"
			s0 "\">" s0
			;; 3. subject
			"\\([^<]*\\(?:\\(?:<[^>]+>\\)?[^<]+\\)+"
			"\\(?:<[^>]+>\\)?\\)"
			s0 "</a>")))
	    nil t)
      (push (shimbun-create-header
	     0
	     (match-string 3)
	     shimbun-nikkei-from-address
	     date
	     (concat "<" (shimbun-subst-char-in-string
			  ?/ ?. (downcase (match-string 1)))
		     "%" group "."
		     shimbun-nikkei-top-level-domain ">")
	     "" 0 0
	     (shimbun-nikkei-expand-url (concat (match-string 1) ".html")
					folder))
	    headers))
    headers))

(defun shimbun-nikkei-get-headers-it-default (group folder shimbun range)
  "Function used to fetch headers for the it groups."
  (let ((pages (shimbun-header-index-pages range))
	(count 0)
	sub-end id headers)
    (catch 'stop
      (while t
	(while (re-search-forward
		(eval-when-compile
		  (let ((s0 "[\t\n ]*")
			(s1 "[\t\n ]+"))
		    (concat "<a" s1 "href=\"/?"
			    ;; 1. url
			    "\\([^\"]+="
			    ;; 2. serial number
			    "\\([^\"/]+"
			    ;; 3. day
			    "\\([0-3][0-9]\\)"
			    ;; 4. month
			    "\\([01][0-9]\\)"
			    ;; 5. year
			    "\\(20[0-9][0-9]\\)"
			    "\\)\\(?:&Page="
			    ;; 6. serial number
			    "\\([0-9]+\\)"
			    "\\)?\\)"
			    s0 "\"" s0 ">"
			    "\\(?:" s0 "([01]?[0-9]/[0-3]?[0-9])\\)?" s0
			    ;; 7. subject
			    "\\([^<]+\\)"
			    s0 "</a>")))
		nil t)
	  (setq sub-end (point)
		id (concat "<" (shimbun-subst-char-in-string
				?  ?_ (downcase (match-string 2)))
			   "%" group "."
			   shimbun-nikkei-top-level-domain ">"))
	  (if (shimbun-search-id shimbun id)
	      (throw 'stop nil)
	    (push (shimbun-create-header
		   0
		   (match-string 7)
		   shimbun-nikkei-from-address
		   (shimbun-nikkei-make-date-string
		    (string-to-number (match-string 5))
		    (string-to-number (match-string 4))
		    (string-to-number (match-string 3)))
		   id "" 0 0
		   (shimbun-nikkei-expand-url (match-string 1) folder))
		  headers)
	    (goto-char sub-end)))
	(if (and (or (not pages)
		     (< (setq count (1+ count)) pages))
		 (re-search-forward "\
<a href=\"\\([^\"]+\\)\">&gt;&gt; $B2a5n5-;v0lMw(B</a>\
\\|<a href=\"\\([^\"]+\\)\">$B<!$X(B&gt;</a>"
				    nil t))
	    (progn
	      (shimbun-retrieve-url (prog1
					(concat "\
http://it.nikkei.co.jp/" (or (match-string 1) (match-string 2)))
				      (erase-buffer))
				    t)
	      (goto-char (point-min)))
	  (throw 'stop nil))))
    headers))

(defun shimbun-nikkei-get-headers-it-pc (group folder shimbun range)
  "Function used to fetch headers for the it.pc group."
  (let ((pages (shimbun-header-index-pages range))
	(count 0)
	id headers)
    (catch 'stop
      (while t
	(while (and (re-search-forward
		     (eval-when-compile
		       (let ((s0 "[\t\n ]*")
			     (s1 "[\t\n ]+"))
			 (concat "<a" s1 "href=\"/"
				 ;; 1. url
				 "\\(pc/news/index\\.aspx\\?ichiran=True&n="
				 ;; 2. serial number
				 "\\([^&]+"
				 ;; 3. year
				 "\\(20[0-9][0-9]\\)"
				 "\\)&Page=[0-9]+\\)"
				 s0 "\"" s0 ">" s0 "("
				 ;; 4. month
				 "\\([01]?[0-9]\\)"
				 s0 "/" s0
				 ;; 5. day
				 "\\([0-3]?[0-9]\\)"
				 s0 ")" s0
				 ;; 6. subject
				 "\\([^<]+\\)"
				 s0 "</a>")))
		     nil t)
		    (not (string-match "\\`[\t\n $B!!(B]*\\'" (match-string 6))))
	  (setq id (concat "<" (shimbun-subst-char-in-string
				?  ?_ (downcase (match-string 2)))
			   "%" group "." shimbun-nikkei-top-level-domain ">"))
	  (if (shimbun-search-id shimbun id)
	      (throw 'stop nil)
	    (push (shimbun-create-header
		   0
		   (match-string 6)
		   shimbun-nikkei-from-address
		   (shimbun-nikkei-make-date-string
		    (string-to-number (match-string 3))
		    (string-to-number (match-string 4))
		    (string-to-number (match-string 5)))
		   id "" 0 0
		   (shimbun-nikkei-expand-url
		    (concat "http://it.nikkei.co.jp/" (match-string 1))
		    folder))
		  headers)))
	(if (and (or (not pages)
		     (< (setq count (1+ count)) pages))
		 (progn
		   (goto-char (point-min))
		   (re-search-forward "<a[\t\n ]+href=\"/\
\\(pc/news/index\\.aspx\\?ichiran=True&Page=[0-9]+\\)[\t\n ]*\"[\t\n ]*>\
\[\t\n ]*$B<!$X(B[\t\n ]*&gt;[\t\n ]*</a>"
				      nil t)))
	    (progn
	      (shimbun-retrieve-url (prog1
					(concat "http://it.nikkei.co.jp/"
						(match-string 1))
				      (erase-buffer))
				    t)
	      (goto-char (point-min)))
	  (throw 'stop nil))))
    headers))

(defun shimbun-nikkei-get-headers-markets (group folder shimbun range)
  "Function used to fetch headers for the markets group."
  (let (headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\"summary\\.cfm"
			;; 1. url
			"\\(\\?genre="
			;; 2. serial number
			"\\([^\"]+date="
			;; 3. year
			"\\(20[0-9][0-9]\\)"
			"[01][0-9][0-3][0-9]"
			"\\)"
			"\\)"
			s0 "\"" s0 ">" s0
			;; 4. subject
			"\\([^<]+\\)"
			s0 "$B!J(B"
			;; 5. month
			"\\([01]?[0-9]\\)"
			"/"
			;; 6. day
			"\\([0-3]?[0-9]\\)"
			s1
			;; 7. hour
			"\\([0-2]?[0-9]\\)"
			":"
			;; 8. minute
			"\\([0-5]?[0-9]\\)"
			"$B!K(B" s0 "</a>")))
	    nil t)
      (push (shimbun-create-header
	     0
	     (match-string 4)
	     shimbun-nikkei-from-address
	     (shimbun-nikkei-make-date-string
	      (string-to-number (match-string 3))
	      (string-to-number (match-string 5))
	      (string-to-number (match-string 6))
	      (format "%02d:%02d"
		      (string-to-number (match-string 7))
		      (string-to-number (match-string 8))))
	     (concat "<" (downcase (match-string 2)) "%" group "."
		     shimbun-nikkei-top-level-domain ">")
	     "" 0 0
	     (shimbun-nikkei-expand-url (match-string 1) folder))
	    headers))
    headers))

(defun shimbun-nikkei-get-headers-kawase (group folder shimbun range)
  "Function used to fetch headers for the kawase group."
  (let (headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\"summary\\.cfm"
			;; 1. url
			"\\(\\?id="
			;; 2. serial number
			"\\([^\"]+date="
			;; 3. year
			"\\(20[0-9][0-9]\\)"
			"[01][0-9][0-3][0-9]"
			"\\)"
			"\\)"
			s0 "\"" s0 ">" s0
			;; 4. subject
			"\\([^<]+\\)"
			s0 "$B!J(B"
			;; 5. month
			"\\([01]?[0-9]\\)"
			"/"
			;; 6. day
			"\\([0-3]?[0-9]\\)"
			s1
			;; 7. hour
			"\\([0-2]?[0-9]\\)"
			":"
			;; 8. minute
			"\\([0-5]?[0-9]\\)"
			"$B!K(B" s0 "</a>")))
	    nil t)
      (push (shimbun-create-header
	     0
	     (match-string 4)
	     shimbun-nikkei-from-address
	     (shimbun-nikkei-make-date-string
	      (string-to-number (match-string 3))
	      (string-to-number (match-string 5))
	      (string-to-number (match-string 6))
	      (format "%02d:%02d"
		      (string-to-number (match-string 7))
		      (string-to-number (match-string 8))))
	     (concat "<" (downcase (match-string 2)) "%" group "."
		     shimbun-nikkei-top-level-domain ">")
	     "" 0 0
	     (shimbun-nikkei-expand-url (match-string 1) folder))
	    headers))
    headers))

(defun shimbun-nikkei-get-headers-bunkatsu2 (group folder shimbun range)
  "Function used to fetch headers for the gyosuuchi group."
  (let (headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\"bunkatsu3\\.cfm\\?genre=m4"
			;; 1. url
			"\\(&id="
			;; 2. serial number
			"\\([^\"]+date="
			;; 3. year
			"\\(20[0-9][0-9]\\)"
			"[01][0-9][0-3][0-9][^\"]+"
			"\\)"
			"\\)"
			s0 "\"" s0 ">" s0
			;; 4. subject
			"\\([^<]+\\)"
			s0 "$B!J(B"
			;; 5. month
			"\\([01]?[0-9]\\)"
			"/"
			;; 6. day
			"\\([0-3]?[0-9]\\)"
			s1
			;; 7. hour
			"\\([0-2]?[0-9]\\)"
			":"
			;; 8. minute
			"\\([0-5]?[0-9]\\)"
			"$B!K(B" s0 "</a>")))
	    nil t)
      (push (shimbun-create-header
	     0
	     (match-string 4)
	     shimbun-nikkei-from-address
	     (shimbun-nikkei-make-date-string
	      (string-to-number (match-string 3))
	      (string-to-number (match-string 5))
	      (string-to-number (match-string 6))
	      (format "%02d:%02d"
		      (string-to-number (match-string 7))
		      (string-to-number (match-string 8))))
	     (concat "<" (downcase (match-string 2)) "%" group "."
		     shimbun-nikkei-top-level-domain ">")
	     "" 0 0
	     (shimbun-nikkei-expand-url (match-string 1) folder))
	    headers))
    headers))

(defun shimbun-nikkei-get-headers-kinri (group folder shimbun range)
  "Function used to fetch headers for the kinri group."
  (let (headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\"kinri\\.cfm"
			;; 1. url
			"\\(\\?id="
			;; 2. serial number
			"\\([^\"]+date="
			;; 3. year
			"\\(20[0-9][0-9]\\)"
			"[01][0-9][0-3][0-9]"
			"\\)"
			"\\)"
			s0 "\"" s0 ">" s0
			;; 4. subject
			"\\([^<]+\\)"
			s0 "$B!J(B"
			;; 5. month
			"\\([01]?[0-9]\\)"
			"/"
			;; 6. day
			"\\([0-3]?[0-9]\\)"
			s1
			;; 7. hour
			"\\([0-2]?[0-9]\\)"
			":"
			;; 8. minute
			"\\([0-5]?[0-9]\\)"
			"$B!K(B" s0 "</a>")))
	    nil t)
      (push (shimbun-create-header
	     0
	     (match-string 4)
	     shimbun-nikkei-from-address
	     (shimbun-nikkei-make-date-string
	      (string-to-number (match-string 3))
	      (string-to-number (match-string 5))
	      (string-to-number (match-string 6))
	      (format "%02d:%02d"
		      (string-to-number (match-string 7))
		      (string-to-number (match-string 8))))
	     (concat "<" (downcase (match-string 2)) "%" group "."
		     shimbun-nikkei-top-level-domain ">")
	     "" 0 0
	     (shimbun-nikkei-expand-url (match-string 1) folder))
	    headers))
    headers))

(defun shimbun-nikkei-get-headers-ft (group folder shimbun range)
  "Function used to fetch headers for the ft group."
  (let (headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\"ft\\.cfm"
			;; 1. url
			"\\(\\?id="
			;; 2. serial number
			"\\([^\"]+date="
			;; 3. year
			"\\(20[0-9][0-9]\\)"
			"[01][0-9][0-3][0-9]"
			"\\)"
			"\\)"
			s0 "\"" s0 ">" s0
			;; 4. subject
			"\\([^<]+\\)"
			s0 "[(|$B!J(B]"
			;; 5. month
			"\\([01]?[0-9]\\)"
			"/"
			;; 6. day
			"\\([0-3]?[0-9]\\)"
			"[$B!K(B|)]" s0 "$B"((B" s0 "</a>")))
	    nil t)
      (push (shimbun-create-header
	     0
	     (match-string 4)
	     shimbun-nikkei-from-address
	     (shimbun-nikkei-make-date-string
	      (string-to-number (match-string 3))
	      (string-to-number (match-string 5))
	      (string-to-number (match-string 6)))
	     (concat "<" (downcase (match-string 2)) "%" group "."
		     shimbun-nikkei-top-level-domain ">")
	     "" 0 0
	     (shimbun-nikkei-expand-url (match-string 1) folder))
	    headers))
    headers))

(defun shimbun-nikkei-get-headers-dj (group folder shimbun range)
  "Function used to fetch headers for the dj group."
  (let (headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\"dj\\.cfm"
			;; 1. url
			"\\(\\?id="
			;; 2. serial number
			"\\([^\"]+date="
			;; 3. year
			"\\(20[0-9][0-9]\\)"
			"[01][0-9][0-3][0-9]"
			"\\)"
			"\\)"
			s0 "\"" s0 ">" s0
			;; 4. subject
			"\\([^<]+\\)"
			s0 "$B!J(B"
			;; 5. month
			"\\([01]?[0-9]\\)"
			"/"
			;; 6. day
			"\\([0-3]?[0-9]\\)"
			s1
			;; 7. hour
			"\\([0-2]?[0-9]\\)"
			":"
			;; 8. minute
			"\\([0-5]?[0-9]\\)"
			"$B!K(B" s0 "</a>")))
	    nil t)
      (push (shimbun-create-header
	     0
	     (match-string 4)
	     shimbun-nikkei-from-address
	     (shimbun-nikkei-make-date-string
	      (string-to-number (match-string 3))
	      (string-to-number (match-string 5))
	      (string-to-number (match-string 6))
	      (format "%02d:%02d"
		      (string-to-number (match-string 7))
		      (string-to-number (match-string 8))))
	     (concat "<" (downcase (match-string 2)) "%" group "."
		     shimbun-nikkei-top-level-domain ">")
	     "" 0 0
	     (shimbun-nikkei-expand-url (match-string 1) folder))
	    headers))
    headers))

(defun shimbun-nikkei-get-headers-gyoseki (group folder shimbun range)
  "Function used to fetch headers for the gyoseki group."
  (let (headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\"gyoseki\\.cfm"
			;; 1. url
			"\\(\\?id="
			;; 2. serial number
			"\\([^\"]+date="
			;; 3. year
			"\\(20[0-9][0-9]\\)"
			"[01][0-9][0-3][0-9]"
			"\\)"
			"\\)"
			s0 "\"" s0 ">" s0
			;; 4. subject
			"\\([^<]+\\)"
			s0 "$B!J(B"
			;; 5. month
			"\\([01]?[0-9]\\)"
			"/"
			;; 6. day
			"\\([0-3]?[0-9]\\)"
			s1
			;; 7. hour
			"\\([0-2]?[0-9]\\)"
			":"
			;; 8. minute
			"\\([0-5]?[0-9]\\)"
			"$B!K(B" s0 "</a>")))
	    nil t)
      (push (shimbun-create-header
	     0
	     (match-string 4)
	     shimbun-nikkei-from-address
	     (shimbun-nikkei-make-date-string
	      (string-to-number (match-string 3))
	      (string-to-number (match-string 5))
	      (string-to-number (match-string 6))
	      (format "%02d:%02d"
		      (string-to-number (match-string 7))
		      (string-to-number (match-string 8))))
	     (concat "<" (downcase (match-string 2)) "%" group "."
		     shimbun-nikkei-top-level-domain ">")
	     "" 0 0
	     (shimbun-nikkei-expand-url (match-string 1) folder))
	    headers))
    headers))

(defun shimbun-nikkei-get-headers-market (group folder shimbun range)
  "Function used to fetch headers for the market group."
  (let ((subregexp
	 (eval-when-compile
	   (let ((s0 "[\t\n ]*")
		 (s1 "[\t\n ]+"))
	     (concat "class=\"sub_bar\"" s0 ">" s0
		     ;; 1. subtitle
		     "\\([^\t\n <]+\\)"
		     ".+class=\"sub_bar_time\"" s0 ">" s0
		     "$B99?7(B" s0 "$B!'(B" s0
		     ;; 2. month
		     "\\([01]?[0-9]\\)"
		     "$B7n(B"
		     ;; 3. day
		     "\\([0-3]?[0-9]\\)"
		     "$BF|(B\\(?:" s1
		     ;; 4. hour:minute
		     "\\([012]?[0-9]:[0-5]?[0-9]\\)"
		     "\\)?"))))
	subdata start end subtitle month day time from year headers)
    (when (re-search-forward subregexp nil t)
      (setq subdata (copy-sequence (match-data))
	    start (point))
      (while start
	(if (re-search-forward subregexp nil t)
	    (progn
	      (setq subdata (prog1
				(copy-sequence (match-data))
			      (set-match-data subdata))
		    end (point))
	      (goto-char start))
	  (set-match-data subdata)
	  (setq end nil))
	(setq subtitle (match-string 1)
	      month (string-to-number (match-string 2))
	      day (string-to-number (match-string 3))
	      time (match-string 4))
	(setq from (shimbun-replace-in-string
		    shimbun-nikkei-from-address
		    ")" (concat "/"
				(shimbun-replace-in-string
				 subtitle "\\(&nbsp;\\)+" "")
				")")))
	(while (re-search-forward
		(eval-when-compile
		  (let ((s0 "[\t\n ]*")
			(s1 "[\t\n ]+"))
		    (concat "<a" s1 "href=\""
			    ;; 1. url
			    "\\([^\">]+/"
			    ;; 2. id
			    "\\("
			    ;; 3. year
			    "\\(20[0-9][0-9]\\)"
			    "[^.]+"
			    "\\)"
			    "\\.html\\)"
			    s0 "\"" s0 ">\\(?:" s0 "<[^>]+>\\)*" s0
			    ;; 4. subject
			    "\\([^<]+\\)"
			    s0)))
		end t)
	  (setq year (string-to-number (match-string 3)))
	  (push (shimbun-create-header
		 0
		 (match-string 4)
		 from
		 (shimbun-nikkei-make-date-string year month day time)
		 (format "<%s%%%s.%s>"
			 (downcase (match-string 2)) group
			 shimbun-nikkei-top-level-domain)
		 "" 0 0
		 (shimbun-nikkei-expand-url (match-string 1)
					    shimbun-nikkei-url))
		headers))
	(setq start end))
      headers)))

(defun shimbun-nikkei-get-headers-china (group folder shimbun range)
  "Function used to fetch headers for the china group."
  (let (headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\""
			;; 1. url
			"\\("
			;; 2. serial number
			"\\("
			;; 3. year
			"\\(20[0-9][0-9]\\)"
			;; 4. month
			"\\([01][0-9]\\)"
			;; 5. day
			"\\([0-3][0-9]\\)"
			"[0-9_a-z]+"
			"\\)"
			"\\.html\\)"
			s0 "\"" s0 ">"
			s0 "\\(?:([01]?[0-9]/[0-3]?[0-9])\\)?" s0
			;; 7. subject
			"\\([^<]+\\)"
			"</a>")))
	    nil t)
      (push (shimbun-create-header
	     0
	     (match-string 6)
	     shimbun-nikkei-from-address
	     (shimbun-nikkei-make-date-string
	      (string-to-number (match-string 3))
	      (string-to-number (match-string 4))
	      (string-to-number (match-string 5)))
	     (concat "<" (downcase (match-string 2)) "%" group "."
		     shimbun-nikkei-top-level-domain ">")
	     "" 0 0
	     (shimbun-nikkei-expand-url (match-string 1) folder))
	    headers))
    headers))

(defun shimbun-nikkei-get-headers-retto (group folder shimbun range)
  "Function used to fetch headers for the retto group."
  (when (re-search-forward "$B!Z(B\\([^\t\n ]+\\)$B![(B" nil t)
    (let ((start (match-end 0))
	  (region (match-string 1))
	  end next subject url serial year month day time headers)
      (while start
	(if (re-search-forward "$B!Z(B\\([^\t\n ]+\\)$B![(B" nil t)
	    (setq end (match-end 0)
		  next (match-string 1))
	  (setq end nil))
	(while (progn
		 (goto-char start)
		 (re-search-forward
		  (eval-when-compile
		    (let ((s0 "[\t\n ]*")
			  (s1 "[\t\n ]+"))
		      (concat "<AREA21" s1 "HEADLINE=\""
			      ;; 1. subject
			      "\\([^\"]+\\)"
			      "\"" s1 "URL=\""
			      ;; 2. url
			      "\\("
			      ;; 3. serial number
			      "\\([^\".]+\\)"
			      "\\.html\\)"
			      s0 "\"" s1 "ARTICLE_TIME=\""
			      ;; 4. year
			      "\\(20[0-9][0-9]\\)"
			      "/"
			      ;; 5. month
			      "\\([01][0-9]\\)"
			      "/"
			      ;; 6. day
			      "\\([0-3][0-9]\\)"
			      s1
			      ;; 7. hour:minute
			      "\\([012][0-9]:[0-5][0-9]\\)")))
		  end t))
	  (setq subject (match-string 1)
		url (match-string 2)
		serial (downcase (match-string 3))
		year (string-to-number (match-string 4))
		month (string-to-number (match-string 5))
		day (string-to-number (match-string 6))
		time (match-string 7)
		start (match-end 0))
	  (when (re-search-forward
		 (concat
		  (eval-when-compile
		    (let ((s0 "[\t\n ]*")
			  (s1 "[\t\n ]+"))
		      (concat "<!--" s1 "aLink" s1 "-->" s0 "<a" s1 "HREF=\""
			      ;; 1. url
			      "\\([^\"]+\\)"
			      s0 "\">" s0 "<!--" s1 "headline" s1 "-->" s0)))
		  (regexp-quote subject))
		 end t)
	    (setq url (match-string 1)))
	  (push (shimbun-create-header
		 0
		 (concat "[" region "] " subject)
		 shimbun-nikkei-from-address
		 (shimbun-nikkei-make-date-string year month day time)
		 (concat "<" serial "%" group "."
			 shimbun-nikkei-top-level-domain ">")
		 "" 0 0
		 (shimbun-nikkei-expand-url url folder))
		headers))
	(setq start end
	      region next))
      headers)))

(defun shimbun-nikkei-get-headers-sports (group folder shimbun range)
  "Function used to fetch headers for the sports group."
  ;; Skip headlinenews.
  (re-search-forward "\
<span[\t\n ]+class=\"sub_bar_time\">[\t\n ]*$B99?7!'(B[01]?[0-9]$B7n(B[0-3]?[0-9]$BF|(B"
		     nil t)
  (let (category headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\""
			;; 1. url
			"\\(http://sports\\.nikkei\\.co\\.jp/news\\.cfm\\?i="
			;; 2. serial number
			"\\("
			;; 3. year
			"\\(20[0-9][0-9]\\)"
			"[^&]+\\)"
			"&t="
			;; 4. category
			"\\([^\"]+\\)"
			"\\)"
			s0 "\">" s0 "("
			;; 5. month
			"\\([01]?[0-9]\\)"
			"/"
			;; 6. day
			"\\([0-3]?[0-9]\\)"
			")" s0
			;; 7. subject
			"\\([^<]+\\)")))
	    nil t)
      (setq category (match-string 4))
      (push (shimbun-create-header
	     0
	     (concat "[" category "] " (match-string 7))
	     shimbun-nikkei-from-address
	     (shimbun-nikkei-make-date-string
	      (string-to-number (match-string 3))
	      (string-to-number (match-string 5))
	      (string-to-number (match-string 6)))
	     (concat "<" (downcase (match-string 2)) "%" category "." group "."
		     shimbun-nikkei-top-level-domain ">")
	     "" 0 0
	     (match-string 1))
	    headers))
    headers))

(defun shimbun-nikkei-get-headers-newpro (group folder shimbun range)
  "Function used to fetch headers for the newpro group."
  (when (re-search-forward ">[\t\n ]*$B!|(B $B?7@=IJ5-;v0lMw(B[\t\n ]*<" nil t)
    (narrow-to-region (point) (or (search-forward "</ul>" nil t)
				  (point-max)))
    (goto-char (point-min))
    (let (headers)
      (while (re-search-forward
	      (eval-when-compile
		(let ((s0 "[\t\n ]*")
		      (s1 "[\t\n ]+"))
		  (concat "<a" s1 "href=\""
			  ;; 1. url
			  "\\(\\(?:[^\"]+/\\)?"
			  ;; 2. serial number
			  "\\("
			  ;; 3. year
			  "\\(20[0-9][0-9]\\)"
			  ;; 4. month
			  "\\([01][0-9]\\)"
			  ;; 5. day
			  "\\([0-3][0-9]\\)"
			  "[0-9a-z]+\\)"
			  "\\.html\\)"
			  s0 "\"" s0 ">" s0
			  ;; 6. subject
			  "\\([^<]+\\)")))
	      nil t)
	(push (shimbun-create-header
	       0
	       (match-string 6)
	       shimbun-nikkei-from-address
	       (shimbun-nikkei-make-date-string
		(string-to-number (match-string 3))
		(string-to-number (match-string 4))
		(string-to-number (match-string 5)))
	       (concat "<" (downcase (match-string 2)) "%" group "."
		       shimbun-nikkei-top-level-domain ">")
	       "" 0 0
	       (shimbun-nikkei-expand-url (match-string 1) folder))
	      headers))
      (widen)
      headers)))

(defun shimbun-nikkei-get-headers-release (group folder shimbun range)
  "Function used to fetch headers for the release group."
  (let (url id subject sub-end year month day headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\""
			;; 1. url
			"\\(detail\\.cfm\\?relID="
			;; 2. serial number
			"\\([^\"]+\\)"
			"\\)"
			s0 "\"" s0 ">" s0
			;; 3. subject
			"\\([^<]+\\)")))
	    nil t)
      (setq url (match-string 1)
	    id (downcase (match-string 2))
	    subject (match-string 3)
	    sub-end (point))
      (when (re-search-backward "\
>[\t\n ]*\\(20[0-9][0-9]\\)/\\([01][0-9]\\)/\\([0-3][0-9]\\)[^0-9]"
				nil t)
	(push (shimbun-create-header
	       0
	       subject
	       shimbun-nikkei-from-address
	       (shimbun-nikkei-make-date-string
		(setq year (string-to-number (match-string 1)))
		(setq month (string-to-number (match-string 2)))
		(setq day (string-to-number (match-string 3))))
	       (format "<%d%02d%02d.%s%%%s.%s>"
		       year month day id group shimbun-nikkei-top-level-domain)
	       "" 0 0
	       (shimbun-nikkei-expand-url url folder))
	      headers)
	(goto-char sub-end)))
    headers))

(defun shimbun-nikkei-get-headers-release2 (group folder shimbun range)
  "Function used to fetch headers for the release-in-detail groups."
  (let ((pages (shimbun-header-index-pages range))
	(count 0)
	url id subject sub-end year month day headers)
    (catch 'stop
      (while t
	(while (re-search-forward
		(eval-when-compile
		  (let ((s0 "[\t\n ]*")
			(s1 "[\t\n ]+"))
		    (concat "<a" s1 "href=\""
			    ;; 1. url
			    "\\(detail\\.cfm\\?relID="
			    ;; 2. serial number
			    "\\([^\"]+\\)"
			    "\\)"
			    s0 "\"" s0 ">" s0
			    ;; 3. subject
			    "\\([^<]+\\)")))
		nil t)
	  (setq url (match-string 1)
		id (downcase (match-string 2))
		subject (match-string 3)
		sub-end (point))
	  (when (re-search-backward "\
>[\t\n ]*\\(20[0-9][0-9]\\)/\\([01][0-9]\\)/\\([0-3][0-9]\\)[^0-9]"
				    nil t)
	    (setq year (string-to-number (match-string 1))
		  month (string-to-number (match-string 2))
		  day (string-to-number (match-string 3))
		  id (format "<%d%02d%02d.%s%%%s.%s>"
			     year month day id group
			     shimbun-nikkei-top-level-domain))
	    (if (shimbun-search-id shimbun id)
		(throw 'stop nil)
	      (push (shimbun-create-header
		     0 subject shimbun-nikkei-from-address
		     (shimbun-nikkei-make-date-string year month day)
		     id "" 0 0
		     (shimbun-nikkei-expand-url
		      (concat "http://release.nikkei.co.jp/" url) folder))
		    headers)
	      (goto-char sub-end))))
	(if (and (or (not pages)
		     (< (setq count (1+ count)) pages))
		 (progn
		   (goto-char (point-min))
		   (re-search-forward "<a[\t\n ]+href=\"\
\\(isclassList\\.cfm\\?page=[0-9]+&lindID=[0-9]+&sindID=[0-9]+\\)\
\[\t\n ]*\"[\t\n ]*>[\t\n ]*$B<!$X(B[\t\n ]*&gt;[\t\n ]*</a>"
				      nil t)))
	    (progn
	      (shimbun-retrieve-url (prog1
					(concat "http://release.nikkei.co.jp/"
						(match-string 1))
				      (erase-buffer))
				    t)
	      (goto-char (point-min)))
	  (throw 'stop nil))))
    headers))

(defun shimbun-nikkei-get-headers-shasetsu (group folder shimbun range)
  "Function used to fetch headers for the shasetsu group."
  (goto-char (point-max))
  (insert (with-temp-buffer
	    (shimbun-retrieve-url
	     "http://www.nikkei.co.jp/news/shasetsu/IndexKijiIchiran.js"
	     t)
	    (buffer-string)))
  (goto-char (point-min))
  (let (headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\\\\\""
			;; 1. url
			"\\(\\(?:[^\"/\\]+/+\\)+"
			;; 2. serial number
			"\\([^\"/\\]+"
			;; 3. year
			"\\(20[0-9][0-9]\\)"
			"\\)"
			"\\.html\\)"
			"\\\\\">" s0
			;; 4. subject
			"\\(\\(?:$B<R@b(B\\|$B=U=)(B\\)[^<]*(" s0
			;; 5. month
			"\\([01]?[0-9]\\)"
			s0 "/" s0
			;; 6. day
			"\\([0-3]?[0-9]\\)"
			s0 ")\\)"
			s0 "</a>")))
	    nil t)
      (push (shimbun-create-header
	     0
	     (match-string 4)
	     shimbun-nikkei-from-address
	     (shimbun-nikkei-make-date-string
	      (string-to-number (match-string 3))
	      (string-to-number (match-string 5))
	      (string-to-number (match-string 6)))
	     (concat "<" (downcase (match-string 2)) "%" group "."
		     shimbun-nikkei-top-level-domain ">")
	     "" 0 0
	     (shimbun-nikkei-expand-url (match-string 1) folder))
	    headers))
    (nreverse headers)))

(luna-define-method shimbun-make-contents :before ((shimbun shimbun-nikkei)
						   header)
  (let ((fn (nth 4 (assoc (shimbun-current-group-internal shimbun)
			  shimbun-nikkei-group-table)))
	(case-fold-search t))
    (while (search-forward "\r" nil t)
      (delete-backward-char 1))
    (goto-char (point-min))
    (while (re-search-forward "[\t\n ]*\\(?:\
<ul\\(?:[\t\n ]+[^>]+\\)?>\
\\|</ul>\
\\|<a[\t\n ]+[^>]+>[\t\n ]*$B!c3HBg(B\\(?:$B2hA|(B\\)?$B!d(B[\t\n ]*</a>\
\\|\\(?:<div[\t\n ]+[^>]+>[\t\n ]*\\)?<img[\t\n ]+src=\"[^\"]+/s\\.gif\"\
\[^>]+>\\(?:[\t\n ]*</div>\\)?\
\\)[\t\n ]*"
			      nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    (goto-char (point-min))
    (unless (and (fboundp fn)
		 (funcall fn header))
      (erase-buffer)
      (insert "<html><body>\
Couldn't extract the body for this article.<br>\
Please visit <a href=\""
	      (shimbun-header-xref header)
	      "\"><u>the original page</u></a>.\
</body></html>\n"))
    (goto-char (point-min))))

(defun shimbun-nikkei-prepare-article-default (&rest args)
  "Default function used to prepare contents of an article."
  (let (photo-end body)
    (when (re-search-forward "<table[\t\n ]+id=\"photonews" nil t)
      (delete-region (point-min) (match-beginning 0))
      (when (search-forward "</table>" nil t)
	(setq photo-end (point))))
    (when (or (and (re-search-forward "\
<!-+[\t\n ]*FJZONE[\t\n ]+START[\t\n ]+NAME=\"HONBUN\"[\t\n ]*-+>[\t\n ]*"
				      nil t)
		   (progn
		     (setq body (point))
		     (re-search-forward "\
\\(?:[\t\n $B!!(B]*<\\(?:p\\|p[\t\n $B!!(B]+[^>]+\\|/p\\|/p[\t\n $B!!(B]+[^>]+\\)>\\)*\
\[\t\n $B!!(B]*<!-+[\t\n ]*FJZONE[\t\n ]+END[\t\n ]+NAME=\"HONBUN\""
					nil t)))
	      ;; The following section will be used for the `main' group.
	      (and (re-search-forward "\
<!-+[\t\n ]*FJZONE[\t\n ]+END[\t\n ]+NAME=\"MIDASHI\""
				      nil t)
		   (search-forward "<p>" nil t)
		   (progn
		     (setq body (match-beginning 0))
		     (re-search-forward "<p[^>]\\|\n\n+" nil t))))
      (goto-char (match-beginning 0))
      (insert shimbun-nikkei-content-end)
      (if photo-end
	  (progn
	    (delete-region photo-end body)
	    ;; Replace <img src='...'> with <img src="...">.
	    (goto-char (point-min))
	    (while (re-search-forward "<img[\t\n ]+src='\\([^\"']+\\)'"
				      nil t)
	      (replace-match "<img src=\"\\1\""))
	    (goto-char (point-min)))
	(goto-char body))
      (insert shimbun-nikkei-content-start)
      t)))

(defun shimbun-nikkei-prepare-article-default2 (&rest args)
  "Function used to prepare contents of an article for some groups."
  ;; Remove unwanted images.
  (let (start end)
    (while (re-search-forward "[\t\n ]*<div[\t\n ]+[^>]+>[\t\n ]*<img[\t\n ]+\
\[^>]+>[\t\n ]*</div>[\t\n ]*"
			      nil t)
      (setq start (match-beginning 0)
	    end (match-end 0))
      (goto-char start)
      (if (re-search-forward
	   "src=\"http://parts\\.nikkei\\.co\\.jp/parts/s\\.gif\""
	   end t)
	  (delete-region start end)
	(goto-char end))))
  (goto-char (point-min))
  (when (re-search-forward "\
<!-+[\t\n ]*FJZONE[\t\n ]+START[\t\n ]+NAME=\"HONBUN\"[\t\n ]*-+>"
			   nil t)
    (insert shimbun-nikkei-content-start)
    (when (re-search-forward "\
\\(?:[\t\n $B!!(B]*<\\(?:p\\|p[\t\n $B!!(B]+[^>]+\\|/p\\|/p[\t\n $B!!(B]+[^>]+\\)>\\)*\
\[\t\n $B!!(B]*<!-+[\t\n ]*FJZONE[\t\n ]+END[\t\n ]+NAME=\"HONBUN\"[\t\n ]*-+>\
\\|<a[\t\n ]+name=\"newslist\"></a>\n"
			     nil t)
      (goto-char (match-beginning 0))
      (insert shimbun-nikkei-content-end)
      t)))

(defun shimbun-nikkei-prepare-article-kansai (&rest args)
  "Function used to prepare contents of an article for the kansai group."
  (when (re-search-forward "\
<td[\t\n ]+colspan=\"2\"[\t\n ]+class=\"textm\">"
			   nil t)
    (insert shimbun-nikkei-content-start)
    (when (re-search-forward "[\t\n ]*\\(?:<[^>]+>[\t\n ]*\\)*\\(\
<!-+[\t\n ]*\\*+[\t\n ]*\\(?:main[\t\n ]+end\\|footer[\t\n ]+begin\\)[\t\n ]*\
\\*+[\t\n ]*-+>\
\\|<table[\t\n ]+border=\"0\"[\t\n ]+cellspacing=\
\"0\"[\t\n ]+cellpadding=\"0\"[\t\n ]+width=\"720\">\\)"
			     nil t)
      (goto-char (match-beginning 0))
      (insert shimbun-nikkei-content-end)
      t)))

(defun shimbun-nikkei-prepare-article-default3 (&rest args)
  "Function used to prepare contents of an article for the market groups."
  (when (re-search-forward "<a[\t\n ]+name=\"midashi\">[^<]*</a>[\t\n ]*"
			   nil t)
    (and (re-search-forward "<a[\t\n ]+href=\"[^\"]+\\.cfm\">[^<]*</a>[\t\n ]*"
			    nil t)
	 (re-search-forward "\
<div\\(?:[\t\n ]+[^>]+\\)*[\t\n ]+\\(?:class=\"news\"\\|id=\"report\"\\)\
\\(?:[\t\n ]+[^>]+\\)*>[\t\n ]*"
			    nil t)
	 (looking-at "<h[0-9]+>[^<]+</h[0-9]+>[\t\n ]*")
	 (goto-char (match-end 0)))
    (insert shimbun-nikkei-content-start)
    (or (re-search-forward "\
<[\t\n ]*p[\t\n ]+align[\t\n ]*=[\t\n ]*right[\t\n ]*>[^<]*\
\([\t\n /0-9:$B!!(B]+)[\t\n ]*</p>"
			   nil t)
	(and (or (re-search-forward "\
\\(?:[\t\n ]*<[^>]+>\\)*[\t\n ]*<div\\(?:[\t\n ]+[^>]+\\)*[\t\n ]+\
class=\"column\"\\(?:[\t\n ]+[^>]+\\)*>"
				    nil t)
		 (re-search-forward "\\(?:[\t\n ]*<[^>]+>\\)*[\t\n ]*\
\\(?:<li>\\|<a[\t\n ]+href=\"/ranking/\\|<img[\t\n ]+src=\\)"
				    nil t))
	     (goto-char (match-beginning 0))))
    (insert shimbun-nikkei-content-end)
    t))

(defun shimbun-nikkei-prepare-article-bunkatsu2 (&rest args)
  "Function used to prepare contents of an article for the gyosuuchi group."
  (when (re-search-forward "[\t\n ]<div[\t\n ]+class=\"bg_gray\">" nil t)
    (insert shimbun-nikkei-content-start)
    (when (re-search-forward "[\t\n ]<div[\t\n ]+class=\"column\">" nil t)
      (goto-char (match-beginning 0))
      (insert shimbun-nikkei-content-end)
      t)))

(defun shimbun-nikkei-prepare-article-sports (&rest args)
  "Function used to prepare contents of an article for the sports group."
  (when (re-search-forward "\
<!-+[\t\n ]*FJZONE[\t\n ]+END[\t\n ]+NAME=\"MIDASHI\"[\t\n ]*-+>"
			   nil t)
    (insert shimbun-nikkei-content-start)
    (when (re-search-forward "\
<!-+[\t\n ]*FJZONE[\t\n ]+END[\t\n ]+NAME=\"HONBUN\"[\t\n ]*-+>"
			     nil t)
      (goto-char (match-beginning 0))
      (insert shimbun-nikkei-content-end)
      t)))

(defun shimbun-nikkei-prepare-article-newpro (&rest args)
  "Function used to prepare contents of an article for the newpro group."
  (let (photo-end body)
    (when (re-search-forward "<table[\t\n ]+id=\"photonews" nil t)
      (delete-region (point-min) (match-beginning 0))
      (when (search-forward "</table>" nil t)
	(setq photo-end (point))))
    (when (and (re-search-forward "\
<!-+[\t\n ]*FJZONE[\t\n ]+START[\t\n ]+NAME=\"HONBUN\"[\t\n ]*-+>[\t\n ]*"
				  nil t)
	       (setq body (point))
	       (re-search-forward "\
\[\t\n ]*<!-+[\t\n ]*FJZONE[\t\n ]+END[\t\n ]+NAME=\"HONBUN\""
				  nil t))
      (goto-char (match-beginning 0))
      (insert shimbun-nikkei-content-end)
      (if photo-end
	  (progn
	    (delete-region photo-end body)
	    (goto-char (point-min)))
	(goto-char body))
      (insert shimbun-nikkei-content-start)
      t)))

(defun shimbun-nikkei-prepare-article-release (&rest args)
  "Function used to prepare contents of an article for the release group."
  (shimbun-remove-tags "<p[\t\n ]+class=\"re_print\"" "</p>")
  (goto-char (point-min))
  (when (re-search-forward "<[\t\n ]*TD[\t\n ]+colspan=\"3\">" nil t)
    (insert shimbun-nikkei-content-start)
    (when (re-search-forward "[\t\n ]*<div[\t\n ]+class=\"tokushu\">" nil t)
      (goto-char (match-beginning 0))
      (insert shimbun-nikkei-content-end)
      t)))

(defun shimbun-nikkei-prepare-article-release2 (&rest args)
  "Function used to prepare contents of an article for the release groups."
  (when (re-search-forward ">[\t\n ]*$B$3$N%Z!<%8$r%W%j%s%H$9$k(B[\t\n ]*\
\\(?:\\(?:<[^>]+>*[\t\n ]*\\)*<h[0-9]+\\(?:[\t\n ]+[^>]+\\)*\
\[\t\n ]+[^\t\n >]+[\t\n ]*=[\t\n ]*\"[\t\n ]*heading[\t\n ]*\"[^>]*>\
\[^<]+</h[0-9]+>\\(?:[\t\n ]*<[^>]+>\\)*[\t\n ]*\\)?"
			   nil t)
    (insert shimbun-nikkei-content-start)
    (and (re-search-forward "[\t\n ]*\\(?:<[^>]+>[\t\n ]*\\)*\
<div\\(?:[\t\n ]+[^>]+\\)*[\t\n ]+class[\t\n ]*=[\t\n ]*\"[\t\n ]*\
tokushu[\t\n ]*\""
			    nil t)
	 (goto-char (match-beginning 0)))
    (insert shimbun-nikkei-content-end)
    t))

(defun shimbun-nikkei-prepare-article-market (header)
  "Function used to prepare contents of an article for the market group."
  (when (re-search-forward "\
<!-+[\t\n ]*FJZONE[\t\n ]+START[\t\n ]+NAME=\"HONBUN\"[\t\n ]*-+>"
			   nil t)
    (insert shimbun-nikkei-content-start)
    (when (re-search-forward "\\(?:(\\([012]?[0-9]:[0-5]?[0-9]\\))[\t\n ]*\\)?\
<!-+[\t\n ]*FJZONE[\t\n ]+END[\t\n ]+NAME=\"HONBUN\"[\t\n ]*-+>"
			     nil t)
      (if (match-beginning 1)
	  (progn
	    (goto-char (1+ (match-end 1)))
	    (let ((new (match-string 1))
		  (date (shimbun-header-date header)))
	      (when (string-match "[012]?[0-9]:[0-5]?[0-9]" date)
		(shimbun-header-set-date
		 header (replace-match new nil nil date)))))
	(goto-char (match-beginning 0)))
      (insert shimbun-nikkei-content-end)
      t)))

(defun shimbun-nikkei-prepare-article-default4 (&rest args)
  "Function used to prepare contents of an article for some groups."
  (when (re-search-forward "\
<!-+[\t\n ]*FJZONE[\t\n ]+START[\t\n ]+NAME=\"HONBUN\"[\t\n ]*-+>"
			   nil t)
    (insert shimbun-nikkei-content-start)
    (when (re-search-forward "\
<!-+[\t\n ]*FJZONE[\t\n ]+END[\t\n ]+NAME=\"HONBUN\"[\t\n ]*-+>"
			     nil t)
      (goto-char (match-beginning 0))
      (insert shimbun-nikkei-content-end)
      t)))

(defun shimbun-nikkei-prepare-article-top (header)
  "Function used to prepare contents of an article for the top group."
  (let (start end)
    (if (or
	 (when (re-search-forward
		"<!-+[\t\n ]*$B%3%s%F%s%DOH(B[\t\n ]*-+>[\t\n ]*"
		nil t)
	   (setq start (match-end 0))
	   (or (re-search-forward "\\(?:[\t\n ]*<[^>]+>\\)*[\t\n ]*\
<!-+[\t\n ]*//$B%3%s%F%s%DOH(B[\t\n ]*-+>[\t\n ]*"
				  nil t)
	       (prog1 nil (goto-char (point-min)))))
	 (when (re-search-forward
		"<!-+[\t\n ]*$BJT@.%3%s%F%s%DOH(B[\t\n ]*-+>[\t\n ]*"
		nil t)
	   (setq start (match-end 0))
	   (or (re-search-forward "\\(?:[\t\n ]*<[^>]+>\\)*[\t\n ]*\
<!-+[\t\n ]*//$BJT@.%3%s%F%s%DOH(B[\t\n ]*-+>[\t\n ]*"
				  nil t)
	       (prog1 nil (goto-char (point-min)))))
	 (when (re-search-forward "<!--photo-->[\t\n ]*" nil t)
	   (setq start (match-end 0))
	   (or (re-search-forward "\
\\(?:[\t\n ]*<[^>]+>\\)*[\t\n ]*\\[[01]?[0-9]$B7n(B[0-3]?[0-9]$BF|(B[/$B!?(B][^]]+\\]"
				  nil t)
	       (prog1 nil (goto-char (point-min)))))
	 (when (re-search-forward "\
<[^>]*[\t\n ]+summary=\"$B<L??%K%e!<%9(B\"[^>]*>\\(?:[\t\n ]*<[^\t\n >]+>\\)*\
\[\t\n ]*\
\\|<!-+[\t\n ]*FJZONE[\t\n ]+START[\t\n ]+NAME[\t\n ]*\
=[\t\n ]*\"[\t\n ]*HONBUN[\t\n ]*\"[\t\n ]*-+>"
				  nil t)
	   (setq start (match-end 0))
	   (or (re-search-forward "\\(?:[\t\n ]*<[^/][^>]*>\\)*[\t\n ]*\
<!-+[\t\n ]*FJZONE[\t\n ]+END[\t\n ]+NAME[\t\n ]*\
=[\t\n ]*\"[\t\n ]*HONBUN[\t\n ]*\"[\t\n ]*-+>"
				  nil t)
	       (prog1 nil (goto-char (point-min)))))
	 (when (or (re-search-forward
		    "<!-+[\t\n ]*$BFC=85-;vBg(B[\t\n ]*-+>[\t\n ]*"
		    nil t)
		   (re-search-forward "<!-+[\t\n ]*$B5-;v(B[\t\n ]*-+>[\t\n ]*"
				      nil t))
	   (setq start (match-end 0))
	   (or (re-search-forward "\\(?:[\t\n ]*<[^>]+>\\)*[\t\n ]*\
<!-+[\t\n ]*\\(?://$B5-;v(B\\|$BFC=85-;v%U%C%?(B\\)[\t\n ]*-+>"
				  nil t)
	       (prog1 nil (goto-char (point-min)))))
	 (setq end (shimbun-nikkei-prepare-article-default header))
	 (prog1 nil (goto-char (point-min)))

	 ;; Filters having a potential to mistakenly extract the body follow.
	 (when (or (re-search-forward "\
<a[\t\n ]+href=\"\\./\">[\t\n ]*$B%H%C%W(B[\t\n ]*</a>[\t\n ]*"
				      nil t)
		   (re-search-forward "\
<div[\t\n ]+class=\"title[^\"]*\">\\(?:[\t\n ]*<[^>]+>\\)*[\t\n ]*"
				      nil t)
		   (re-search-forward "\
<a[\t\n ]+href=\"\\./\">\\(?:[\t\n ]*<[^>]+>\\)*[\t\n ]*</a>\
\\(?:[\t\n ]*<[^>]+>\\)*[\t\n ]*"
				      nil t))
	   (setq start (match-end 0))
	   (or (re-search-forward "\
\[\t\n ]*\\(?:<[^>]+>[\t\n ]*\\)*<p\\(?:[\t\n ]+[^>]+\\)*[\t\n ]align="
				  nil t)
	       (prog1 nil (goto-char (point-min)))))
	 (when (or (re-search-forward "\
<!-+[\t\n ]*$B<L??(B[\t\n ]*-+>\\(?:[\t\n ]*<[^i][^>]*>\\)*[\t\n ]*"
				      nil t)
		   (re-search-forward "<!-+[\t\n ]*$BK\J8(B[\t\n ]*-+>" nil t)
		   (re-search-forward "<div[\t\n ]+class=[^>]+>[\t\n ]*"
				      nil t))
	   (setq start (match-end 0))
	   (set-match-data nil)
	   (while (re-search-forward "[\t\n ]*\\(?:<[^>]+>[\t\n ]*\\)*\
<!-+[\t\n ]*$BK\J8(B[\t\n ]*-+>"
				     nil t))
	   (or (match-beginning 0)
	       (prog1 nil (goto-char (point-min))))))
	(progn
	  (unless end
	    (goto-char (match-beginning 0))
	    (insert shimbun-nikkei-content-end)
	    (goto-char start)
	    (insert shimbun-nikkei-content-start))
	  t)
      (when (and (re-search-forward "\
<a[\t\n ]+[^>]+>[\t\n ]*$B!d!d5-;v$rFI$`(B[\t\n ]*</a>"
				    nil t)
		 (re-search-backward "href=\"\\([^\"]+\\)"
				     (match-beginning 0) t))
	(let ((new (match-string 1))
	      (old (shimbun-header-xref header)))
	  (when (string-match "[^/]/[^/]" old)
	    (setq new (shimbun-nikkei-expand-url
		       new (substring old 0 (1- (match-end 0)))))
	    (shimbun-header-set-xref header new)
	    (erase-buffer)
	    (shimbun-retrieve-url new t)
	    (goto-char (point-min))
	    (shimbun-nikkei-prepare-article-top header)))))))

(provide 'sb-nikkei)

;;; sb-nikkei.el ends here
