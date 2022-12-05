/* VERSION 9/1/2005 */

/* SCRIPT BY MICHAEL MARTIN */


rem set termout off
set pause off
set verify off
set pages 600
set linesize 80

prompt This script will not run if you have not run biomass yet.

/* Query user for area and year */
	accept xsurvey_area prompt 'Enter survey area (GOA, AI or WINTER): '
	accept xyear prompt 'Enter survey year (e.g. 1993): '
	define cpuetab = 'cpue'
	define sz = 'sizecomp_'
	define st = 'stratum_'
	define in = 'inpfc_'
	define d = 'depth_'
	define ad = 'area_depth_'
	define ar = 'area_'
	define t = 'total_'
	define szsty = &sz&st&xyear
	define sziny = &sz&in&xyear
	define szindy = &sz&in&d&xyear
	define szdy = &sz&d&xyear
	define szady = &sz&ad&xyear
	define szary = &sz&ar&xyear
	define szty = &sz&t&xyear

/* Create table of relevant catchjoins */
	drop table catchjoins;

	prompt Making catchjoins table...
	create table catchjoins as select distinct catchjoin
	from &cpuetab c, goa.analysis_species s
	where s.species_code = c.species_code 
	and (s.sizecomp_flag = upper('&xsurvey_area') or s.sizecomp_flag = 'BOTH') and c.year = &xyear
	and c.survey = upper('&xsurvey_area');
	
/* Create length table for sizecomp etc. */

	drop table temp_length;

	create table temp_length as select cruisejoin, hauljoin, c.catchjoin,  
	vessel, cruise, haul, s.species_code, length, frequency, sex
	from racebase.length l, goa.analysis_species s, catchjoins c
	where l.species_code = s.species_code
	and (s.sizecomp_flag = upper('&xsurvey_area') or s.sizecomp_flag = 'BOTH') 
	and l.catchjoin = c.catchjoin;
		
	
	drop table temp_cpue;

	create table temp_cpue as select ca.catchjoin, hauljoin, vessel, cruise, haul, stratum, 
	species_code, numcpue from catchjoins ca, &cpuetab c where 
	ca.catchjoin = c.catchjoin and numcpue > 0;
	

Prompt Creating table no_length_hauls...

drop table no_length_hauls_1;

create table no_length_hauls_1 as select nvl(l.catchjoin, -1) flag, c.catchjoin, 
	c.hauljoin, c.vessel, c.cruise, c.haul, c.species_code, c.stratum 
	from temp_cpue c, temp_length l
	where c.catchjoin = l.catchjoin(+);
	
drop table no_length_hauls;

create table no_length_hauls as 
	select catchjoin, hauljoin, vessel, cruise, haul, species_code, stratum
	from no_length_hauls_1 where flag = -1;

drop table count_hauls1;

create table count_hauls1 as select n.stratum, n.species_code,
	count(n.hauljoin) count_catch
	from no_length_hauls n, temp_cpue c
	where n.stratum = c.stratum
	and n.species_code = c.species_code
	group by n.stratum, n.species_code;

drop table count_hauls2;

create table count_hauls2 as select stratum, species_code,
	count(hauljoin) count_nolen
	from no_length_hauls n
	group by n.stratum, n.species_code;

drop table count_hauls;

create table count_hauls as select n.stratum, n.species_code, count_catch - count_nolen count
	from count_hauls1 c, count_hauls2 n
	where c.stratum = n.stratum and c.species_code = n.species_code;


/* THIS SECTION CALCULATES THE RATIO OF EACH HAUL CPUE TO THE */
/* TOTAL STRATUM CPUE */


prompt Creating table lfnumsum...

drop table lfnumsum;

create table lfnumsum as 
select stratum, species_code, sum(numcpue) sumcpue
from temp_cpue 
group by stratum, species_code;

drop table cpueratio;

Prompt Creating table cpueratio...

create table cpueratio as
select hauljoin, vessel, cruise, haul, b.stratum, c.species_code,
numcpue/sumcpue cprat from temp_cpue c, lfnumsum b
where c.stratum=b.stratum and c.species_code = b.species_code;

/* THIS SECTION GETS THE STRATUM POPULATION. */

drop table areapop;
Prompt Creating table areapop...
create table areapop as
select c.survey, c.year, g.summary_area, g.summary_depth, g.summary_area_depth, g.regulatory_area_name, 
s.species_code, c.stratum, stratum_pop
 from biomass_stratum c, goa.analysis_species s, goa.goa_strata g 
where c.stratum = g.stratum and c.species_code = s.species_code
and (s.sizecomp_flag = upper('&xsurvey_area') or s.sizecomp_flag = 'BOTH') 
and g.survey = upper('&xsurvey_area') and c.year = &xyear;


/* THIS SECTION SUMS THE LENGTHS TAKEN BY HAUL AND PUTS IT IN */
/* A table CALLED TOT */

drop table tot;
Prompt Creating table tot...
create table tot as
select hauljoin, vessel, cruise, haul, species_code, sum(frequency) totbyhaul
from temp_length 
group by hauljoin, vessel, cruise, haul, species_code;

/*  THIS SECTION PUTS THE LENGTH AND TOT TABLES TOGETHER */
/*  TO GET A RATIO OF FREQ TO TOTBYHAUL FOR MALES AND MAKES A table */

drop table ratiomale;
Prompt Creating table ratiomale...
create table ratiomale as
select l.hauljoin,l.vessel,l.cruise,l.haul, l.species_code, 
l.length,sex,frequency/totbyhaul ratiom from tot t,
temp_length l where sex = 1 and
t.hauljoin=l.hauljoin and t.species_code = l.species_code;


/* NOW FEMALES */

drop table ratiofemale;
Prompt Creating table ratiofemale...
create table ratiofemale as
select l.hauljoin,l.vessel,l.cruise,l.haul, l.species_code, 
l.length,sex,frequency/totbyhaul ratiof from tot t,
temp_length l where sex = 2 and
t.hauljoin=l.hauljoin and t.species_code = l.species_code;


/* NOW UNSEXED */

drop table ratiounsex;
Prompt Creating table ratiounsex...
create table ratiounsex as
select l.hauljoin,l.vessel,l.cruise,l.haul, l.species_code, 
l.length,sex,frequency/totbyhaul ratiou from tot t,
temp_length l where sex = 3 and
t.hauljoin=l.hauljoin and t.species_code = l.species_code;


drop table masterlen;
Prompt Creating table masterlen...
create table masterlen as
select distinct hauljoin,vessel,cruise,haul,species_code, length from temp_length;


drop table addstratm;
Prompt Creating table addstratm...
create table addstratm as
select l.hauljoin, l.vessel, l.cruise, l.haul, l.species_code, l.length,
nvl(ratiom,0.0) ratiom
from ratiomale m,masterlen l
where l.hauljoin = m.hauljoin(+) and
m.length(+) = l.length
and m.species_code(+) = l.species_code;


drop table addstratf;
Prompt Creating table addstratf...
create table addstratf as
select l.hauljoin, l.vessel, l.cruise, l.haul, l.species_code, l.length, 
nvl(ratiof,0.0) ratiof 
from ratiofemale f,masterlen l
where l.hauljoin = f.hauljoin(+) and
f.length(+) = l.length 
and f.species_code(+) = l.species_code;


drop table addstratu;
Prompt Creating table addstratu...
create table addstratu as
select l.hauljoin,l.vessel,l.cruise,l.haul,l.species_code, l.length, 
nvl(ratiou,0.0) ratiou
from ratiounsex u,masterlen l
where l.hauljoin = u.hauljoin(+) and
u.length(+) = l.length 
and u.species_code(+) = l.species_code;


drop table totallen;
Prompt Creating table totallen...
create table totallen STORAGE (INITIAL 1M NEXT 1M) as 
select l.hauljoin,l.vessel,l.cruise,l.haul,l.species_code, l.length,ratiom,ratiof,
ratiou,stratum from temp_cpue c, masterlen l,addstratm m,addstratf f,addstratu u where 
l.hauljoin = m.hauljoin and l.hauljoin = f.hauljoin
and l.hauljoin = u.hauljoin and l.hauljoin = c.hauljoin and
l.length = f.length and l.length = m.length and l.length = u.length
and l.species_code = m.species_code and l.species_code = f.species_code
and l.species_code = u.species_code and l.species_code = c.species_code;

drop table totallen2;

create table totallen2 STORAGE (INITIAL 1M NEXT 1M) as select c.hauljoin, c.vessel, c.cruise, c.haul, 
	c.species_code, l.length, sum(ratiom)/count ratiom, 
	sum(ratiof)/count ratiof, 
	sum(ratiou)/count ratiou, c.stratum
	from no_length_hauls c, totallen l, count_hauls ch
	where c.species_code = l.species_code and c.species_code = ch.species_code
	and c.stratum = l.stratum and c.stratum = ch.stratum and ch.count > 0
	group by c.hauljoin, c.vessel, c.cruise, c.haul, c.stratum, c.species_code, l.length, count;

drop table totallen3;

create table totallen3 as select hauljoin, species_code, sum(ratiom + ratiof + ratiou) total
	from totallen2 group by hauljoin, species_code;
	
drop table totallen4;

create table totallen4 STORAGE (INITIAL 1M NEXT 1M) as select t2.hauljoin, t2.vessel, t2.cruise, t2.haul, 
	t2.species_code, t2.length, ratiom/total ratiom, ratiof/total ratiof,
	ratiou/total ratiou, t2.stratum from totallen2 t2, totallen3 t3
	where t2.hauljoin = t3.hauljoin and t2.species_code = t3.species_code;

insert into totallen select * from totallen4;

/* THIS SECTION MULTIPLIES RATIO OF CPUES TIMES RATIO OF LENGTHS */
/* TIMES POPULATION AND SUMS OVER STRATUM FOR EACH LENGTH */

drop table &szsty;
Prompt Creating table &szsty...
create table &szsty STORAGE (INITIAL 1 M) as
select p.survey, p.year, r.species_code, p.stratum,r.length,
round(sum(cprat * ratiom * stratum_pop),0) males,
round(sum(cprat * ratiof * stratum_pop),0) females,
round(sum(cprat * ratiou * stratum_pop),0) unsexed,
round(((sum(cprat*ratiom*stratum_pop))+(sum(cprat*ratiof*stratum_pop))+
(sum(cprat*ratiou*stratum_pop))),0)total
from totallen r, cpueratio c, areapop p where 
r.hauljoin=c.hauljoin and r.stratum = p.stratum
and r.species_code = c.species_code and r.species_code = p.species_code
group by p.survey, p.year, p.stratum,r.species_code, length;

delete from sizecomp_stratum where year = &xyear;

insert into sizecomp_stratum (select * from &szsty);

drop table &sziny;
Prompt Creating table &sziny...
create table &sziny STORAGE (INITIAL 1 M) as
select p.survey, p.year, r.species_code,p.summary_area,r.length,
round(sum(cprat * ratiom * stratum_pop),0) males,
round(sum(cprat * ratiof * stratum_pop),0) females,
round(sum(cprat * ratiou * stratum_pop),0) unsexed,
round(((sum(cprat*ratiom*stratum_pop))+(sum(cprat*ratiof*stratum_pop))+
(sum(cprat*ratiou*stratum_pop))),0) total
from totallen r, cpueratio c, areapop p where 
r.hauljoin=c.hauljoin and r.stratum = p.stratum
and r.species_code = c.species_code and r.species_code = p.species_code
group by p.survey, p.year, summary_area,r.species_code, length;

delete from sizecomp_inpfc where year = &xyear;

insert into sizecomp_inpfc (select * from &sziny);

drop table &szdy;
Prompt Creating table &szdy...
create table &szdy STORAGE (INITIAL 1 M) as
select p.survey, p.year, r.species_code,p.summary_depth,r.length,
round(sum(cprat * ratiom * stratum_pop),0) males,
round(sum(cprat * ratiof * stratum_pop),0) females,
round(sum(cprat * ratiou * stratum_pop),0) unsexed,
round(((sum(cprat*ratiom*stratum_pop))+(sum(cprat*ratiof*stratum_pop))+
(sum(cprat*ratiou*stratum_pop))),0) total
from totallen r, cpueratio c, areapop p where 
r.hauljoin=c.hauljoin and r.stratum = p.stratum
and r.species_code = c.species_code and r.species_code = p.species_code
group by p.survey, p.year, summary_depth,r.species_code, length;

delete from sizecomp_depth where year = &xyear;

insert into sizecomp_depth (select * from &szdy);

drop table &szindy;
Prompt Creating table &szindy...
create table &szindy STORAGE (INITIAL 1 M) as
select p.survey, p.year, r.species_code,p.summary_area_depth,r.length,
round(sum(cprat * ratiom * stratum_pop),0) males,
round(sum(cprat * ratiof * stratum_pop),0) females,
round(sum(cprat * ratiou * stratum_pop),0) unsexed,
round(((sum(cprat*ratiom*stratum_pop))+(sum(cprat*ratiof*stratum_pop))+
(sum(cprat*ratiou*stratum_pop))),0) total
from totallen r, cpueratio c, areapop p where 
r.hauljoin=c.hauljoin and r.stratum = p.stratum
and r.species_code = c.species_code and r.species_code = p.species_code
group by p.survey, p.year, summary_area_depth,r.species_code, length;

delete from sizecomp_inpfc_depth where year = &xyear;

insert into sizecomp_inpfc_depth (select * from &szindy);

drop table &szary;
Prompt Creating table &szary...
create table &szary STORAGE (INITIAL 1 M) as
select p.survey, p.year, r.species_code,p.regulatory_area_name,r.length,
round(sum(cprat * ratiom * stratum_pop),0) males,
round(sum(cprat * ratiof * stratum_pop),0) females,
round(sum(cprat * ratiou * stratum_pop),0) unsexed,
round(((sum(cprat*ratiom*stratum_pop))+(sum(cprat*ratiof*stratum_pop))+
(sum(cprat*ratiou*stratum_pop))),0) total
from totallen r, cpueratio c, areapop p where 
r.hauljoin=c.hauljoin and r.stratum = p.stratum
and r.species_code = c.species_code and r.species_code = p.species_code
group by p.survey, p.year, regulatory_area_name, r.species_code, length;

delete from sizecomp_area where year = &xyear;

insert into sizecomp_area (select * from &szary);

drop table &szty;

Prompt Creating table &szty...
create table &szty STORAGE (INITIAL 1 M) as
select p.survey, p.year, r.species_code, 999 "SUMMARY_AREA",r.length,
round(sum(cprat * ratiom * stratum_pop),0) males,
round(sum(cprat * ratiof * stratum_pop),0) females,
round(sum(cprat * ratiou * stratum_pop),0) unsexed,
round(((sum(cprat*ratiom*stratum_pop))+(sum(cprat*ratiof*stratum_pop))+
(sum(cprat*ratiou*stratum_pop))),0) total
from totallen r, cpueratio c, areapop p where 
r.hauljoin = c.hauljoin and r.stratum = p.stratum
and r.species_code = c.species_code and r.species_code = p.species_code
group by p.survey, p.year, r.species_code, length;

delete from sizecomp_total where year = &xyear;

insert into sizecomp_total (select * from &szty);




drop table &szady;
Prompt Creating table &szady...
create table &szady STORAGE (INITIAL 1 M) as
select p.survey, p.year, r.species_code, p.regulatory_area_name, p.summary_depth, r.length,
round(sum(cprat * ratiom * stratum_pop),0) males,
round(sum(cprat * ratiof * stratum_pop),0) females,
round(sum(cprat * ratiou * stratum_pop),0) unsexed,
round(((sum(cprat*ratiom*stratum_pop))+(sum(cprat*ratiof*stratum_pop))+
(sum(cprat*ratiou*stratum_pop))),0) total
from totallen r, cpueratio c, areapop p where 
r.hauljoin=c.hauljoin and r.stratum = p.stratum
and r.species_code = c.species_code and r.species_code = p.species_code
group by p.survey, p.year, regulatory_area_name, summary_depth, r.species_code, length;

delete from sizecomp_area_depth where year = &xyear;

insert into sizecomp_area_depth (select * from &szady);

drop table &szty;

Prompt Creating table &szty...
create table &szty STORAGE (INITIAL 1 M) as
select p.survey, p.year, r.species_code, 999 "SUMMARY_AREA",r.length,
round(sum(cprat * ratiom * stratum_pop),0) males,
round(sum(cprat * ratiof * stratum_pop),0) females,
round(sum(cprat * ratiou * stratum_pop),0) unsexed,
round(((sum(cprat*ratiom*stratum_pop))+(sum(cprat*ratiof*stratum_pop))+
(sum(cprat*ratiou*stratum_pop))),0) total
from totallen r, cpueratio c, areapop p where 
r.hauljoin = c.hauljoin and r.stratum = p.stratum
and r.species_code = c.species_code and r.species_code = p.species_code
group by p.survey, p.year, r.species_code, length;

delete from sizecomp_total where year = &xyear;

insert into sizecomp_total (select * from &szty);






drop table catchjoins;
drop table temp_length;
drop table temp_cpue;
drop table count_hauls1;
drop table count_hauls2;
drop table count_hauls;
drop table no_length_hauls_1;
drop table no_length_hauls;
drop table lfnumsum;
drop table cpueratio;
drop table areapop;
drop table tot;
drop table ratiomale;
drop table ratiofemale;
drop table ratiounsex;
drop table masterlen;
drop table addstratm;
drop table addstratf;
drop table addstratu;
drop table totallen;
drop table totallen2;
drop table totallen3;
drop table totallen4;
drop table &szsty;
drop table &sziny;
drop table &szindy;
drop table &szdy;
drop table &szary;
drop table &szady;
drop table &szty;
