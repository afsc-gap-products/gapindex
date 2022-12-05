/* VERSION 12/03/2012 */

/* SCRIPT BY MICHAEL MARTIN*/
/* modified by N. Laman to eliminate all skate eggs when creating Catch2 */
/* modified by W. Palsson on 10/29/`5 to make duration test >= and not just >  */
set verify off

/* Query user for area and year */
	accept xsurvey_area prompt 'Enter survey area (GOA, AI or WINTER): '
	accept xyear prompt 'Enter survey year (e.g. 2001): '
	accept xdur prompt 'Enter minimum duration for acceptable tow (in minutes): '
	define cpue = 'cpue_'
	define b = 'biomass_'
	define s = 'stratum_'
	define in = 'inpfc_'
	define a = 'area_'
	define t = 'total_'
	define d = 'depth_'
	define cpuet = &cpue&xyear
	define bsy = &b&s&xyear
	define bindy = &b&in&d&xyear
	define bdy = &b&d&xyear
	define biny = &b&in&xyear
	define bay = &b&a&xyear
        define bady = &b&a&d&xyear
	define bty = &b&t&xyear

/* Create table of relevant cruisejoins */
	drop table cruisejoins;

	prompt Making table cruisejoins...

	create table cruisejoins as select cruisejoin from goa.biennial_surveys 
	where cruise between (&xyear * 100) and ((&xyear * 100) + 99)
	and survey = upper('&xsurvey_area');
		
/* Create temporary haul table */
	drop table hauls;

	prompt Gathering haul data...

	create table hauls as select 
		hauljoin, vessel, cruise, haul, stratum, 
		distance_fished, net_width,
		(distance_fished*(net_width/1000)) effort from 
		racebase.haul h, cruisejoins c where
		h.cruisejoin =c.cruisejoin and 
		performance >=0 and 
		haul_type = 3 and duration >= &xdur/60 and gear between 160 and 172
		and stratum is not null and stratum <> 0; 

	insert into hauls (select hauljoin, vessel, cruise, haul, stratum, 
		distance_fished, net_width,
		(distance_fished*(net_width/1000)) from 
		racebase.haul h, cruisejoins c where
		h.cruisejoin =c.cruisejoin and 
		performance >=0 and 
		haul_type = 3 and duration >= &xdur/60 and gear between 710 and 717
		and stratum is not null);
		
	drop table temp_strata;

	create table temp_strata as select * from goa.goa_strata where survey = upper('&xsurvey_area') and stratum in
		(select distinct stratum from hauls);
		
/* Delete hauls that are not in current survey design */
		
	delete from hauls where stratum not in (select stratum from temp_strata);


/* Create temporary catch table */

	prompt Gathering catch data...

	drop table catch1;

	create table catch1 as select 
		h.hauljoin, catchjoin, c.species_code, c.weight,
		c.number_fish from 
		racebase.catch c, hauls h where 
		h.hauljoin =c.hauljoin and species_code in 
		(select species_code from goa.analysis_species
		where (biomass_flag = upper('&xsurvey_area') or biomass_flag = 'BOTH')
		and start_code is null);
	
	drop table catch2;

	create table catch2 as select 
		h.hauljoin, 0 catchjoin, s.species_code, s.summary_group, sum(c.weight) weight,
		sum(c.number_fish) number_fish from 
		racebase.catch c, hauls h, goa.analysis_species s where 
		h.hauljoin =c.hauljoin and
		-- added 12/03/12 by N. Laman to keep skate eggs out of cpue and biomass estimates 
		(c.species_code between s.start_code and s.end_code and c.species_code not in (
			select species_code
			from racebase.species
			where species_code between 400 and 499 
			and (common_name like '%egg%' or 
            species_name like '%egg%')
			))
			and (s.biomass_flag = upper('&xsurvey_area') or biomass_flag = 'BOTH')
		group by h.hauljoin, s.species_code, s.summary_group;

	drop table aggregate_species;		

	create table aggregate_species as select not_to_include, hauljoin, sum(weight) weight,
		sum(number_fish) number_fish 
		from catch1 c, goa.analysis_species a
		where not_to_include is not null and c.species_code =  a.species_code
		group by hauljoin, not_to_include;

	delete from catch1 where species_code between 401 and 403;

	drop table catch3;

	create table catch3 as select c.hauljoin, c.catchjoin, c.species_code, 
		c.weight - nvl(a.weight,0) weight,
		c.number_fish - nvl(a.number_fish,0) number_fish 
		from catch2 c, aggregate_species a 
		where c.summary_group = a.not_to_include(+) and c.hauljoin = a.hauljoin(+);
		
	drop table catches;

	create table catches as 
		select * from catch1
		union
		select * from catch3;

	delete from catches where weight = 0;

	create index catches_hauljoins_species 
	on catches(hauljoin, catchjoin, species_code) tablespace RACE_WORK_IDX;
	
/* Create preliminary cpue table with correct number of rows */

	drop table species1;
	
	create table species1 as select * from goa.analysis_species
		where (biomass_flag = upper('&xsurvey_area') or 
		biomass_flag = 'BOTH') and species_code not in (401, 402, 403);		

	drop table cpue1;

	prompt Making table cpue1...

	create table cpue1 STORAGE (INITIAL 1 M) as select 
		h.hauljoin, h.vessel, h.cruise, h.haul, 
		h.stratum, h.distance_fished, h.net_width, s.species_code, h.effort 
		from hauls h, species1 s;

	create index cpue1_hauljoin_species on cpue1(hauljoin, species_code)
		STORAGE (INITIAL 2M) tablespace RACE_WORK_IDX;

/* Create cpue table */
	drop table &cpuet;

	prompt Making table &cpuet...

	create table &cpuet STORAGE (INITIAL 1 M) as select upper('&xsurvey_area') survey, &xyear year,
		c.catchjoin, c1.hauljoin, c1.vessel, c1.cruise, c1.haul, 
		c1.stratum, c1.distance_fished, c1.net_width, c1.species_code, 
		NVL(c.weight,0.0) weight, NVL(c.number_fish,0) number_fish,
		c1.effort, NVL((c.weight/c1.effort), 0.0) wgtcpue,
		NVL ((c.number_fish/c1.effort), 0) numcpue 
		from cpue1 c1, catches c 
		where c1.species_code = c.species_code(+) and 
		c1.hauljoin=c.hauljoin(+);
	
	drop table no_catch_species;

	create table no_catch_species as select species_code, sum(weight) total_weight 
		from &cpuet group by species_code;
		
	delete from &cpuet 
		where species_code in (select species_code from no_catch_species 
		where total_weight = 0 and (species_code between 10260 and 10262 
		or species_code between 30150 and 30152));
	
	
	delete from cpue where year = &xyear and survey = upper('&xsurvey_area');
	
	insert into cpue (select * from &cpuet);
	
    Clean up intermediate tables */

	drop table cruisejoins;
	drop table species1;
	drop table catch1;
	drop table catch2;
	drop table catch3;
	drop table catches;
	drop table cpue1;

/* Create biomass by stratum table */
/* this section counts hauls, hauls w/catch, hauls w/length for a data summary */

	prompt Now inserting records into biomass by stratum tables...

	prompt Making table hcount...
	drop table hcount;
	create table hcount as
	select stratum, count(distinct hauljoin) haul_count from &cpuet 
	group by stratum;

	drop table ccount1;
	
	create table ccount1 as select stratum, species_code 
		from hcount h, goa.analysis_species g
		where (biomass_flag = upper('&xsurvey_area') or biomass_flag = 'BOTH');
	
	prompt Making table ccount...
	
	drop table ccount2;
	
	create table ccount2 as
		select stratum, species_code, count(distinct hauljoin) catch_count 
		from &cpuet c 
		where wgtcpue > 0 group by stratum, species_code;

	drop table ccount;
	
	create table ccount as 
		select c1.stratum, c1.species_code, nvl(c2.catch_count,0) catch_count
		from ccount1 c1, ccount2 c2 
		where c1.stratum = c2.stratum(+) and c1.species_code = c2.species_code(+);

	prompt Making table hccount...

	drop table hccount;

	create table hccount as
		select h.stratum, c.species_code, h.haul_count, c.catch_count
		from hcount h, ccount c 
		where h.stratum=c.stratum 
		group by h.stratum, c.species_code, h.haul_count, c.catch_count;

	Prompt Making table stratlist;

	drop table stratlist;

	create table stratlist as
		select h.stratum, h.species_code, haul_count, catch_count,  
		avg(wgtcpue) mean_wgt_cpue, variance(wgtcpue)*(1/haul_count) var_wgt_cpue, 
		variance(wgtcpue/1000)*(1/haul_count) biomass_var,
		avg(numcpue) mean_num_cpue, variance(numcpue)*(1/haul_count) var_num_cpue
		from &cpuet c, hccount h
		where h.stratum = c.stratum and h.species_code = c.species_code
		group by h.stratum, h.species_code, haul_count, catch_count
		having haul_count > 1;
	
	create index stratlist_stratum_species on stratlist(stratum, species_code)
		STORAGE (INITIAL 2M) tablespace RACE_WORK_IDX;	
	
	prompt Making table stratlist2...
	
	drop table stratlist2;

	create table stratlist2 as
		select h.stratum, h.species_code, haul_count, catch_count,  
		avg(wgtcpue) mean_wgt_cpue, 0 var_wgt_cpue, 0 biomass_var,
		avg(numcpue) mean_num_cpue, 0 var_num_cpue
		from &cpuet c, hccount h
		where h.stratum = c.stratum and h.species_code = c.species_code
		group by h.stratum, h.species_code, haul_count, catch_count
		having haul_count = 1;
	
	update stratlist2 set var_wgt_cpue=NULL, biomass_var = NULL, var_num_cpue=NULL;
	
	create index stratlist2_stratum_species on stratlist2(stratum, species_code)
		tablespace RACE_WORK_IDX;
		
	prompt Making table &bsy...

	drop table &bsy;
	
	drop table biomass;
	
	create table biomass as
		select s.stratum, species_code, haul_count, catch_count, mean_wgt_cpue, 
		var_wgt_cpue, mean_num_cpue, var_num_cpue, area, 
		(mean_wgt_cpue*area)/1000 stratum_biomass, 
		mean_num_cpue*area stratum_pop,  
		power(area,2)*(biomass_var) biomass_var, 
		power(area,2)*(var_num_cpue) pop_var 	
		from stratlist s, temp_strata g
		where s.stratum = g.stratum;

	drop table stratum_biomass_1;
	
	create table stratum_biomass_1 as
		select stratum, species_code, haul_count, catch_count, 	
		round(mean_wgt_cpue, 1) mean_wgt_cpue, 
		var_wgt_cpue, round(mean_num_cpue,1) mean_num_cpue, var_num_cpue,
		round(stratum_biomass,1) stratum_biomass, biomass_var, 
		round(stratum_biomass-(ninety_five*sqrt(biomass_var)), 1) min_biomass,
		round(stratum_biomass+(ninety_five*sqrt(biomass_var)), 1) max_biomass,
		round(stratum_pop, 0) stratum_pop, pop_var, 
		round(stratum_pop-(ninety_five*sqrt(pop_var)), 0) min_pop,
		round(stratum_pop+(ninety_five*sqrt(pop_var)), 0) max_pop 	
		from biomass b, goa.ttable t
		where (haul_count-1) >= t.degreef_from and (haul_count - 1) < degreef_to
	union 
	select s.stratum, s.species_code, haul_count, catch_count, 
		round(mean_wgt_cpue,0) mean_wgt_cpue, 
		var_wgt_cpue, round(mean_num_cpue,0) mean_num_cpue, var_num_cpue, 
		round((mean_wgt_cpue*area)/1000,0) stratum_biomass, 0 biomass_var, 
		0 min_biomass, 0 max_biomass, round((mean_num_cpue*area),0) stratum_pop, 
		0 pop_var, 0 min_pop, 0 max_pop 	
		from stratlist2 s, temp_strata g
		where s.stratum = g.stratum;

	update stratum_biomass_1 set var_wgt_cpue = NULL, var_num_cpue = NULL, biomass_var = NULL,
		min_biomass = NULL,max_biomass = NULL,pop_var = NULL,min_pop = NULL,max_pop = NULL
		where haul_count = 1;

	update stratum_biomass_1 set min_biomass = 0 where min_biomass < 0;

	update stratum_biomass_1 set min_pop = 0 where min_pop < 0;
		
	drop table &bsy;

	create table &bsy as select upper('&xsurvey_area') survey, &xyear year, stratum,species_code,
		haul_count,catch_count,mean_wgt_cpue,var_wgt_cpue,mean_num_cpue,
		var_num_cpue,stratum_biomass,biomass_var,min_biomass,max_biomass,
		stratum_pop,pop_var,min_pop,max_pop 
		from stratum_biomass_1;
		
/* Insert new records into the biomass by stratum table */

	delete from biomass_stratum where year = &xyear and survey = upper('&xsurvey_area');
	
	insert into biomass_stratum (select * from &bsy);
	
/* Create biomass by inpfc area and depth table */

	prompt Making table &bindy...

	drop table haul_count;
		
	create table haul_count as select summary_area_depth, count(haul) haul_count 
		from hauls h, temp_strata g
		where h.stratum = g.stratum
		group by summary_area_depth;

	drop table f1;

	create table f1 as select h.stratum, 
		((g.area/avg(distance_fished*(net_width/1000)))* 
		((g.area/avg(distance_fished*(net_width/1000))) - b.haul_count))
		/b.haul_count g
		from hauls h, temp_strata g, &bsy b
		where h.stratum = g.stratum and
		h.stratum = b.stratum
		group by h.stratum, area, haul_count;

	drop table area_areas;
	
	create table area_areas as select summary_area_depth, 
		sum(area) area
		from temp_strata
		group by summary_area_depth;
		
	drop table biodeg;

	create table biodeg as select summary_area_depth, species_code,
		round(power(sum(g * biomass_var),2)/sum((power(g,2)*
		power(biomass_var,2))/haul_count - 1),0) deg 
		from &bsy b, temp_strata g, f1 where 
		g.stratum = f1.stratum and g.stratum = b.stratum
		group by summary_area_depth, species_code;

	drop table one_haul_areas;

	create table one_haul_areas as select summary_area_depth, species_code, 
		sum(haul_count) haul_count
		from &bsy b, temp_strata g where b.stratum = g.stratum
		group by summary_area_depth, species_code;

	delete from one_haul_areas where haul_count > 1;

	insert into biodeg (select summary_area_depth, species_code, 0 
		from one_haul_areas);

	drop table inpfc_depth_biomass_1;
	
	create table inpfc_depth_biomass_1 as 
		select b.survey, b.year, a.summary_area_depth, b.species_code, 
		h.haul_count, sum(catch_count) catch_count, 
		sum(mean_wgt_cpue * g.area)/a.area mean_wgt_cpue,
		sum(power((g.area/a.area),2) * var_wgt_cpue) var_wgt_cpue,
		sum(mean_num_cpue * g.area)/a.area mean_num_cpue, 
		sum(power((g.area/a.area),2) * var_num_cpue) var_num_cpue,
		sum(stratum_biomass) area_biomass, sum(biomass_var) biomass_var,
		sum(stratum_pop) area_pop, sum(pop_var) pop_var
		from &bsy b, area_areas a, temp_strata g, haul_count h 
		where g.stratum = b.stratum and 
		g.summary_area_depth = a.summary_area_depth and g.summary_area_depth = h.summary_area_depth
		group by b.survey, b.year, h.haul_count, a.summary_area_depth, a.area, b.species_code;

	drop table &bindy;

	create table &bindy as select b.survey, b.year, b.summary_area_depth, 
		b.species_code, b.haul_count, b.catch_count, b.mean_wgt_cpue, b.var_wgt_cpue,
		b.mean_num_cpue, b.var_num_cpue, b.area_biomass, b.biomass_var, 
		round(b.area_biomass-(t.ninety_five*sqrt(b.biomass_var)), 0) min_biomass,
		round(b.area_biomass+(t.ninety_five*sqrt(b.biomass_var)), 0) max_biomass,
		b.area_pop, b.pop_var, 
		round(b.area_pop-(t.ninety_five*sqrt(b.pop_var)), 0) min_pop,
		round(b.area_pop+(t.ninety_five*sqrt(b.pop_var)), 0) max_pop
		from inpfc_depth_biomass_1 b, goa.ttable t, biodeg d
		where  d.summary_area_depth = b.summary_area_depth and
		d.species_code = b.species_code and  
		d.deg >= degreef_from and d.deg < degreef_to;
	
	update &bindy set var_wgt_cpue = NULL, var_num_cpue = NULL, biomass_var = NULL, 
		min_biomass = NULL, max_biomass = NULL, pop_var = NULL, min_pop = NULL, 
		max_pop = NULL 
		where haul_count = 1;
	update &bindy set min_biomass = 0  where min_biomass < 0;
	update &bindy set min_pop = 0 where min_pop < 0;

/* Insert new records into the biomass by inpfc and depth table */

	delete from biomass_inpfc_depth where year = &xyear and survey = upper('&xsurvey_area');
	
	insert into biomass_inpfc_depth (select * from &bindy);

/* Create biomass by depth table */

	prompt Making table &bdy...

	drop table haul_count;
		
	create table haul_count as select summary_depth, count(haul) haul_count 
		from hauls h, temp_strata g
		where h.stratum = g.stratum
		group by summary_depth;
		
	drop table area_areas;
	
	create table area_areas as select summary_depth, 
		sum(area) area
		from temp_strata
		group by summary_depth;
		
	drop table biodeg;

	create table biodeg as select summary_depth, species_code,
		round(power(sum(g * biomass_var),2)/sum((power(g,2)*
		power(biomass_var,2))/haul_count - 1),0) deg 
		from &bsy b, temp_strata g, f1 where 
		g.stratum = f1.stratum and g.stratum = b.stratum
		group by summary_depth, species_code;

	drop table depth_biomass_1;
	
	create table depth_biomass_1 as select b.survey, b.year, a.summary_depth, b.species_code, 
		h.haul_count, sum(catch_count) catch_count, 
		sum(mean_wgt_cpue * g.area)/a.area mean_wgt_cpue,
		sum(power((g.area/a.area),2) * var_wgt_cpue) var_wgt_cpue,
		sum(mean_num_cpue * g.area)/a.area mean_num_cpue, 
		sum(power((g.area/a.area),2) * var_num_cpue) var_num_cpue,
		sum(stratum_biomass) area_biomass, sum(biomass_var) biomass_var,
		sum(stratum_pop) area_pop, sum(pop_var) pop_var
		from &bsy b, area_areas a, temp_strata g, haul_count h 
		where g.stratum = b.stratum and 
		g.summary_depth = a.summary_depth and g.summary_depth = h.summary_depth
		group by b.survey, b.year, h.haul_count, a.summary_depth, a.area, b.species_code;

	drop table &bdy;

	create table &bdy as select b.survey, b.year, b.summary_depth, 
		b.species_code, b.haul_count, b.catch_count, b.mean_wgt_cpue, b.var_wgt_cpue,
		b.mean_num_cpue, b.var_num_cpue, b.area_biomass, b.biomass_var, 
		round(b.area_biomass-(t.ninety_five*sqrt(b.biomass_var)), 0) min_biomass,
		round(b.area_biomass+(t.ninety_five*sqrt(b.biomass_var)), 0) max_biomass,
		b.area_pop, b.pop_var, 
		round(b.area_pop-(t.ninety_five*sqrt(b.pop_var)), 0) min_pop,
		round(b.area_pop+(t.ninety_five*sqrt(b.pop_var)), 0) max_pop
		from depth_biomass_1 b, goa.ttable t, biodeg d
		where  d.summary_depth = b.summary_depth and
		d.species_code = b.species_code and  
		d.deg >= degreef_from and d.deg < degreef_to;
		
	update &bdy set var_wgt_cpue = NULL, var_num_cpue = NULL, biomass_var = NULL, 
		min_biomass = NULL, max_biomass = NULL, pop_var = NULL, min_pop = NULL, 
		max_pop = NULL 
		where haul_count = 1;
	update &bdy set min_biomass = 0  where min_biomass < 0;
	update &bdy set min_pop = 0 where min_pop < 0;

/* Insert new records into the biomass by depth table */

	delete from biomass_depth where year = &xyear and survey = upper('&xsurvey_area');
	
	insert into biomass_depth (select * from &bdy);

/* Create biomass by inpfc table */

	prompt Making table &biny...

	drop table haul_count;
		
	create table haul_count as select summary_area, count(haul) haul_count 
		from hauls h, temp_strata g
		where h.stratum = g.stratum
		group by summary_area;

	drop table area_areas;
	
	create table area_areas as select summary_area, sum(area) area
		from temp_strata 
		group by summary_area;
		
	drop table biodeg;

	create table biodeg as select summary_area, species_code,
		round(power(sum(g * biomass_var),2)/sum((power(g,2)*
		power(biomass_var,2))/haul_count - 1),0) deg 
		from &bsy b, temp_strata g, f1 where 
		g.stratum = f1.stratum and g.stratum = b.stratum
		group by summary_area, species_code;

	drop table inpfc_biomass_1;
	
	create table inpfc_biomass_1 as select b.survey, b.year, a.summary_area, b.species_code, 
		h.haul_count, sum(catch_count) catch_count, 
		sum(mean_wgt_cpue * g.area)/a.area mean_wgt_cpue,
		sum(power((g.area/a.area),2) * var_wgt_cpue) var_wgt_cpue,
		sum(mean_num_cpue * g.area)/a.area mean_num_cpue, 
		sum(power((g.area/a.area),2) * var_num_cpue) var_num_cpue,
		sum(stratum_biomass) area_biomass, sum(biomass_var) biomass_var,
		sum(stratum_pop) area_pop, sum(pop_var) pop_var
		from &bsy b, area_areas a, temp_strata g, haul_count h 
		where g.stratum = b.stratum and g.summary_area = a.summary_area 
		and g.summary_area = h.summary_area
		group by b.survey, b.year, h.haul_count, a.summary_area, a.area, b.species_code;

	drop table &biny;

	create table &biny as select b.survey, b.year, b.summary_area, 
		b.species_code, b.haul_count, b.catch_count, b.mean_wgt_cpue, b.var_wgt_cpue,
		b.mean_num_cpue, b.var_num_cpue, b.area_biomass, b.biomass_var, 
		round(b.area_biomass-(t.ninety_five*sqrt(b.biomass_var)), 0) min_biomass,
		round(b.area_biomass+(t.ninety_five*sqrt(b.biomass_var)), 0) max_biomass,
		b.area_pop, b.pop_var, 
		round(b.area_pop-(t.ninety_five*sqrt(b.pop_var)), 0) min_pop,
		round(b.area_pop+(t.ninety_five*sqrt(b.pop_var)), 0) max_pop
		from inpfc_biomass_1 b, goa.ttable t, biodeg d
		where d.summary_area = b.summary_area and
		d.species_code = b.species_code and  
		d.deg >= degreef_from and d.deg < degreef_to;
		
	update &biny set var_wgt_cpue = NULL, var_num_cpue = NULL, biomass_var = NULL, 
		min_biomass = NULL, max_biomass = NULL, pop_var = NULL, min_pop = NULL, 
		max_pop = NULL 
		where haul_count = 1;
	update &biny set min_biomass = 0  where min_biomass < 0;
	update &biny set min_pop = 0 where min_pop < 0;

/* Insert new records into the biomass by depth table */

	delete from biomass_inpfc where year = &xyear and survey = upper('&xsurvey_area');
	
	insert into biomass_inpfc (select * from &biny);
	
		
/* Create biomass by area table */

	prompt Making table &bay...

	drop table haul_count;

	create table haul_count as select regulatory_area_name, count(haul) haul_count 
		from hauls h, temp_strata g
		where h.stratum = g.stratum
		group by regulatory_area_name;

	drop table area_areas;
	
	create table area_areas as select regulatory_area_name, 
		sum(area) area 
		from temp_strata 
		group by regulatory_area_name;
		
	drop table biodeg;

	create table biodeg as select regulatory_area_name, species_code,
		round(power(sum(g * biomass_var),2)/sum((power(g,2)*
		power(biomass_var,2))/haul_count - 1),0) deg 
		from &bsy b, temp_strata g, f1 where 
		g.stratum = f1.stratum and g.stratum = b.stratum
		group by regulatory_area_name, species_code;

	drop table area_biomass_1;
	
	create table area_biomass_1 as select b.survey, b.year, a.regulatory_area_name, b.species_code, 
		h.haul_count, sum(catch_count) catch_count, 
		sum(mean_wgt_cpue * g.area)/a.area mean_wgt_cpue,
		sum(power((g.area/a.area),2) * var_wgt_cpue) var_wgt_cpue,
		sum(mean_num_cpue * g.area)/a.area mean_num_cpue, 
		sum(power((g.area/a.area),2) * var_num_cpue) var_num_cpue,
		sum(stratum_biomass) area_biomass, sum(biomass_var) biomass_var,
		sum(stratum_pop) area_pop, sum(pop_var) pop_var
		from &bsy b, area_areas a, temp_strata g, haul_count h 
		where g.stratum = b.stratum and 
		g.regulatory_area_name = a.regulatory_area_name 
		and g.regulatory_area_name = h.regulatory_area_name
		group by b.survey, b.year, h.haul_count, a.regulatory_area_name, a.area, b.species_code;

	drop table &bay;

	create table &bay as select b.survey, b.year, b.regulatory_area_name, 
		b.species_code, b.haul_count, b.catch_count, b.mean_wgt_cpue, b.var_wgt_cpue,
		b.mean_num_cpue, b.var_num_cpue, b.area_biomass, b.biomass_var, 
		round(b.area_biomass-(t.ninety_five*sqrt(b.biomass_var)), 0) min_biomass,
		round(b.area_biomass+(t.ninety_five*sqrt(b.biomass_var)), 0) max_biomass,
		b.area_pop, b.pop_var, 
		round(b.area_pop-(t.ninety_five*sqrt(b.pop_var)), 0) min_pop,
		round(b.area_pop+(t.ninety_five*sqrt(b.pop_var)), 0) max_pop
		from area_biomass_1 b, goa.ttable t, biodeg d
		where  d.regulatory_area_name = b.regulatory_area_name and
		d.species_code = b.species_code and  
		d.deg >= degreef_from and d.deg < degreef_to;
		
	update &bay set var_wgt_cpue = NULL, var_num_cpue = NULL, biomass_var = NULL, min_biomass = NULL,
		max_biomass = NULL, pop_var = NULL, min_pop = NULL, max_pop = NULL 
		where haul_count = 1;
	update &bay set min_biomass = 0  where min_biomass < 0;
	update &bay set min_pop = 0 where min_pop < 0;

/* Insert new records into the biomass by area table */

	delete from biomass_area where year = &xyear and survey = upper('&xsurvey_area');
	
	insert into biomass_area (select * from &bay);
		
/* Create biomass area depth table */

	prompt Making table &bady...

	drop table haul_count;

	create table haul_count as select regulatory_area_name, summary_depth, count(haul) haul_count 
		from hauls h, temp_strata g
		where h.stratum = g.stratum
		group by regulatory_area_name, summary_depth;

	drop table area_areas;
	
	create table area_areas as select regulatory_area_name, summary_depth,
		sum(area) area 
		from temp_strata 
		group by regulatory_area_name, summary_depth;
		
	drop table biodeg;

	create table biodeg as select regulatory_area_name, summary_depth, species_code,
		round(power(sum(g * biomass_var),2)/sum((power(g,2)*
		power(biomass_var,2))/haul_count - 1),0) deg 
		from &bsy b, temp_strata g, f1 where 
		g.stratum = f1.stratum and g.stratum = b.stratum
		group by regulatory_area_name, summary_depth, species_code;

	drop table area_biomass_1;
	
	create table area_biomass_1 as select b.survey, b.year, a.regulatory_area_name, a.summary_depth, b.species_code, 
		h.haul_count, sum(catch_count) catch_count, 
		sum(mean_wgt_cpue * g.area)/a.area mean_wgt_cpue,
		sum(power((g.area/a.area),2) * var_wgt_cpue) var_wgt_cpue,
		sum(mean_num_cpue * g.area)/a.area mean_num_cpue, 
		sum(power((g.area/a.area),2) * var_num_cpue) var_num_cpue,
		sum(stratum_biomass) area_biomass, sum(biomass_var) biomass_var,
		sum(stratum_pop) area_pop, sum(pop_var) pop_var
		from &bsy b, area_areas a, temp_strata g, haul_count h 
		where g.stratum = b.stratum and 
		g.regulatory_area_name = a.regulatory_area_name 
                and g.summary_depth = a.summary_depth
		and g.regulatory_area_name = h.regulatory_area_name
                and g.summary_depth = h.summary_depth
		group by b.survey, b.year, h.haul_count, a.regulatory_area_name, a.summary_depth, a.area, b.species_code;

	drop table &bady;

	create table &bady as select b.survey, b.year, b.regulatory_area_name, b.summary_depth, 
		b.species_code, b.haul_count, b.catch_count, b.mean_wgt_cpue, b.var_wgt_cpue,
		b.mean_num_cpue, b.var_num_cpue, b.area_biomass, b.biomass_var, 
		round(b.area_biomass-(t.ninety_five*sqrt(b.biomass_var)), 0) min_biomass,
		round(b.area_biomass+(t.ninety_five*sqrt(b.biomass_var)), 0) max_biomass,
		b.area_pop, b.pop_var, 
		round(b.area_pop-(t.ninety_five*sqrt(b.pop_var)), 0) min_pop,
		round(b.area_pop+(t.ninety_five*sqrt(b.pop_var)), 0) max_pop
		from area_biomass_1 b, goa.ttable t, biodeg d
		where  d.regulatory_area_name = b.regulatory_area_name and
		d.summary_depth = b.summary_depth and
                d.species_code = b.species_code and  
		d.deg >= degreef_from and d.deg < degreef_to;
		
	update &bdy set var_wgt_cpue = NULL, var_num_cpue = NULL, biomass_var = NULL, min_biomass = NULL,
		max_biomass = NULL, pop_var = NULL, min_pop = NULL, max_pop = NULL 
		where haul_count = 1;
	update &bady set min_biomass = 0  where min_biomass < 0;
	update &bady set min_pop = 0 where min_pop < 0;

/* Insert new records into the biomass by area table */

	delete from biomass_area_depth where year = &xyear and survey = upper('&xsurvey_area');
	
	insert into biomass_area_depth (select * from &bady);
		
/* Create total biomass table */
		
	prompt Making table &bty...

	drop table haul_count;

	create table haul_count as select count(*) haul_count from hauls;
		
	drop table area_areas;
	
	create table area_areas as select  
		sum(area) area
		from temp_strata;

	drop table biodeg;

	create table biodeg as select species_code,
		round(power(sum(g * biomass_var),2)/sum((power(g,2)*
		power(biomass_var,2))/haul_count - 1),0) deg 
		from &bsy b, f1 where 
		b.stratum = f1.stratum
		group by species_code;

	drop table total_biomass_1;
	
	create table total_biomass_1 as select b.survey, b.year, b.species_code, h.haul_count, 
		sum(catch_count) catch_count, 
		sum(mean_wgt_cpue * g.area)/a.area mean_wgt_cpue,
		sum(power((g.area/a.area),2) * var_wgt_cpue) var_wgt_cpue,
		sum(mean_num_cpue * g.area)/a.area mean_num_cpue, 
		sum(power((g.area/a.area),2) * var_num_cpue) var_num_cpue,
		sum(stratum_biomass) total_biomass, sum(biomass_var) biomass_var,
		sum(stratum_pop) total_pop, sum(pop_var) pop_var
		from &bsy b, area_areas a, temp_strata g, haul_count h
		where g.stratum = b.stratum
		group by b.survey, b.year, h.haul_count, a.area, b.species_code;

	drop table &bty;

	create table &bty as select b.survey, b.year, 
		b.species_code, b.haul_count, b.catch_count, b.mean_wgt_cpue, b.var_wgt_cpue,
		b.mean_num_cpue, b.var_num_cpue, b.total_biomass, b.biomass_var, 
		round(b.total_biomass-(t.ninety_five*sqrt(b.biomass_var)), 0) min_biomass,
		round(b.total_biomass+(t.ninety_five*sqrt(b.biomass_var)), 0) max_biomass,
		b.total_pop, b.pop_var, 
		round(b.total_pop-(t.ninety_five*sqrt(b.pop_var)), 0) min_pop,
		round(b.total_pop+(t.ninety_five*sqrt(b.pop_var)), 0) max_pop
		from total_biomass_1 b, goa.ttable t, biodeg d
		where d.species_code = b.species_code and  
		d.deg >= degreef_from and d.deg < degreef_to;
		
	update &bty set var_wgt_cpue = NULL, var_num_cpue = NULL, biomass_var = NULL, 
		min_biomass = NULL, max_biomass = NULL, pop_var = NULL, min_pop = NULL, 
		max_pop = NULL 
		where haul_count = 1;
	update &bty set min_biomass = 0  where min_biomass < 0;
	update &bty set min_pop = 0 where min_pop < 0;
	
/* Insert new records into the biomass by area table */

	delete from biomass_total where year = &xyear and survey = upper('&xsurvey_area');
	
	insert into biomass_total (select * from &bty);
	

/* Clean up intermediate tables */

	drop table temp_strata;
	drop table total_biomass_1;
	drop table area_biomass_1;
	drop table inpfc_biomass_1;	
	drop table one_haul_areas;
	drop table inpfc_depth_biomass_1;
	drop table depth_biomass_1;
	drop table stratum_biomass_1;
	drop table hauls;
	drop table haul_count;
	drop table area_areas;
	drop table f1;
	drop table biodeg;
	drop table biomass;
	drop table ccount1;
	drop table ccount2;
	drop table ccount;
	drop table hcount;
	drop table hccount;
	drop table stratlist;
	drop table stratlist2;	
	drop table &cpuet;
	drop table &bsy;
	drop table &bindy;
	drop table &bdy;
	drop table &biny;
	drop table &bay;
        drop table &bady;
	drop table &bty;


/* Inform the user of the species excluded from the biomass calculations due to no catches recorded*/

	set heading off
	
	prompt The following species were dropped from the biomass run because no catches were recorded:

	select common_name from racebase.species where species_code in 
		(select species_code from no_catch_species where total_weight = 0 and 
		(species_code between 10260 and 10262 or species_code between 30150 and 30152));

	set heading on

	drop table no_catch_species;	
