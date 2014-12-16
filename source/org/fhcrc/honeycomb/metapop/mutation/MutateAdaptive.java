package org.fhcrc.honeycomb.metapop.mutation;

import org.fhcrc.honeycomb.metapop.Population;
import org.fhcrc.honeycomb.metapop.Subpopulation;
import org.fhcrc.honeycomb.metapop.RandomNumberUser;
import org.fhcrc.honeycomb.metapop.fitness.FitnessCalculator;
import org.fhcrc.honeycomb.metapop.fitness.MonodCalculator;
import java.util.List;

/* 
This class mutates one locus of a cell's genotype randomly and places it in a Subpopulation accordingly.
Created August, 2014 by Caroline Cannistra
*/

public class MutateAdaptive implements MutationRule {
	
	public static final double ID_ROUND = 0.001;
	public static final double PRECISION = 0.01;
	public static final double CHEAT_ADV = 1.2;
	private double mut_rate; // rate of mutation
	private double coop_to_cheat_rate; //rate of mutation from cooperator to cheater
	private double cheat_to_coop_rate;
	private RandomNumberUser rng; // random number generator

	//Constructor
	public MutateAdaptive(double mut_rate, double coop_to_cheat_rate, double cheat_to_coop_rate, RandomNumberUser rng) {
		this.mut_rate = mut_rate;
		this.coop_to_cheat_rate = coop_to_cheat_rate;
		this.cheat_to_coop_rate = cheat_to_coop_rate;
		this.rng = rng;
	}

	//Mutates cells in a list of Populations by assigning them new MonodCalculator fields
	//and placing them in new Subpopulations.
	public void mutate(List<Population> pops) {
		for (Population pop:pops) {
			for (Subpopulation sub:pop.copySubpopulations(pop.getSubpopulations())) {
				if (sub.getId().substring(0,4).equals("coop") && coop_to_cheat_rate != 0) {
					int coop_to_cheat = rng.getNextBinomial(sub.getSize(), coop_to_cheat_rate);
					sub.setSize(sub.getSize() - coop_to_cheat);
					double cheater_max = (sub.getFitnessCalculator().getMaxGrowthRate() - 
						sub.getFitnessCalculator().calculateDeathRate(0)) * 200 * CHEAT_ADV;
					String cheater_id = "cheat_" + String.format("%.3g", cheater_max) + sub.getId().substring(sub.getId().lastIndexOf("_"),sub.getId().length());
					if (pop.getSubpopById(cheater_id) != null) {
						pop.getSubpopById(cheater_id).setSize(pop.getSubpopById(cheater_id).getSize() + coop_to_cheat);
					} else {
						double[] params = {CHEAT_ADV, 1, 1};
						pop.addNewSubpopulation(new Subpopulation(coop_to_cheat, sub.getGamma(), 0, 
							sub.getFitnessCalculator().copyFitnessCalculator(params), cheater_id, this.rng));
						System.out.println("New subpop: " + cheater_id + " in " + pop.getCoordinate().toString());
					}
				}
				if (sub.getId().substring(0,5).equals("cheat") && cheat_to_coop_rate != 0) {
					int cheat_to_coop = rng.getNextBinomial(sub.getSize(), cheat_to_coop_rate);
					sub.setSize(sub.getSize() - cheat_to_coop);
					double coop_max = (sub.getFitnessCalculator().getMaxGrowthRate() - 
						sub.getFitnessCalculator().calculateDeathRate(0)) * 200 / CHEAT_ADV;
					String coop_id = "coop_" + String.format("%.3g", coop_max) + sub.getId().substring(sub.getId().lastIndexOf("_"),sub.getId().length());
					if (pop.getSubpopById(coop_id) != null) {
						pop.getSubpopById(coop_id).setSize(pop.getSubpopById(coop_id).getSize() + cheat_to_coop);
					} else {
						double[] params = {1/CHEAT_ADV, 1, 1};
						pop.addNewSubpopulation(new Subpopulation(cheat_to_coop, sub.getGamma(), 2.4, 
							sub.getFitnessCalculator().copyFitnessCalculator(params), coop_id, this.rng));
						System.out.println("New subpop: " + coop_id + " in " + pop.getCoordinate().toString());
					}
				}
				if (mut_rate != 0) {
					int mutants = rng.getNextBinomial(sub.getSize(), mut_rate);
					sub.setSize(sub.getSize() - mutants);
					for (int i=0; i<mutants; i++) {
						double vmax_factor = 1.0;
						double km_factor = 1.0;
						vmax_factor = rng.getNextDouble(0.7, 1);
						km_factor = 3*vmax_factor - 2;
						double new_vmax = vmax_factor*0.45;
						double new_km = km_factor*10;
						double[] params = {vmax_factor, km_factor, 1};
						String new_sub_id = 
							sub.getId().substring(0, sub.getId().indexOf("_")) + "_" + String.format("%.3g", new_vmax) + "_" + String.format("%.3g", new_km);
						if (pop.getSubpopById(new_sub_id) != null) {
							pop.getSubpopById(new_sub_id).setSize(pop.getSubpopById(new_sub_id).getSize() + 1);
						} else {
							pop.addNewSubpopulation(new Subpopulation(1, sub.getGamma(), sub.getReleaseRate(), 
								new MonodCalculator(new_vmax, new_km, sub.getFitnessCalculator().calculateDeathRate(0), 200), new_sub_id, this.rng));
							System.out.println("New subpop: " + new_sub_id + " in " + pop.getCoordinate().toString());
						}
					}
				}
			}
		}
	}

    @Override
    public String toString() {
        return String.format("%s, mut_rate=%.2e",
                             this.getClass().getSimpleName(), mut_rate, coop_to_cheat_rate);
    }
}
