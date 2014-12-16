/**
 * Copyright 2014 Adam Waite
 *
 * This file is part of metapop.
 *
 * metapop is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.  
 *
 * metapop is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with metapop.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.fhcrc.honeycomb.metapop.experiment;

import org.fhcrc.honeycomb.metapop.Subpopulation;
import org.fhcrc.honeycomb.metapop.Population;
import org.fhcrc.honeycomb.metapop.World;
import org.fhcrc.honeycomb.metapop.RandomNumberUser;

import org.fhcrc.honeycomb.metapop.dilution.DilutionRule;

import org.fhcrc.honeycomb.metapop.environment.EnvironmentChanger;
import org.fhcrc.honeycomb.metapop.environment.StaticEnvironment;

import org.fhcrc.honeycomb.metapop.fitness.FitnessCalculator;
import org.fhcrc.honeycomb.metapop.fitness.MonodCalculator;

import org.fhcrc.honeycomb.metapop.coordinate.Coordinate;
import org.fhcrc.honeycomb.metapop.coordinate.picker.CoordinatePicker;
import org.fhcrc.honeycomb.metapop.coordinate.picker.RandomPicker;
import org.fhcrc.honeycomb.metapop.coordinate.picker.UniqueRandomPicker;
import org.fhcrc.honeycomb.metapop.coordinate.picker.RandomNeighborPicker;

import org.fhcrc.honeycomb.metapop.mutation.MutationRule;
import org.fhcrc.honeycomb.metapop.mutation.NoMutation;
import org.fhcrc.honeycomb.metapop.mutation.MutateCoopCheat;
import org.fhcrc.honeycomb.metapop.mutation.MutateAdaptive;

import org.fhcrc.honeycomb.metapop.migration.MigrationRule;
import org.fhcrc.honeycomb.metapop.migration.PropaguleMigration;
import org.fhcrc.honeycomb.metapop.migration.IndividualMigration;

import org.fhcrc.honeycomb.metapop.stop.StopCondition;
import org.fhcrc.honeycomb.metapop.stop.ExtinctOrGrowingStop;
import org.fhcrc.honeycomb.metapop.stop.CoopCheatExtinctStop;
import org.fhcrc.honeycomb.metapop.stop.CheatExtinctStop;
import org.fhcrc.honeycomb.metapop.stop.CoopExtinctStop;
import org.fhcrc.honeycomb.metapop.stop.AllExtinctStop;

import java.io.File;
import java.util.Arrays;
import java.util.List;
import java.util.ArrayList;
import java.util.UUID;

/** 
 * Runs the adaptive race.
 * Created on 6 May, 2013.
 *
 * @author Adam Waite
 * @version $Rev: 2393 $, $Date: 2014-05-24 19:17:59 -0400 (Sat, 24 May 2014) $, $Author: ajwaite $
 *
 */
public abstract class MutationAR {
    public static final int TIMESTEP_SCALE = 200;
    private static final boolean EXCLUDE = true;
    private static final boolean INCLUDE = false;

    private String[] args;

    // Seeds
    private long population_seed;
    private long location_seed;
    private long migration_seed;
    private long mutation_seed;
    private long env_change_seed;

    // RNGs
    private RandomNumberUser population_rng;
    private RandomNumberUser location_rng;
    private RandomNumberUser migration_rng;
    //private RandomNumberUser env_change_rng;
    private RandomNumberUser mutation_rng;

    // Population params
    private int initial_pop_size;
    private double initial_resource;
    private double initial_coop_freq;
    private double frac_occupied;
    private int initial_n_pops;
    private double mutant_freq;
    private double cheat_to_coop_mutation_rate;
    private double coop_to_cheat_mutation_rate;
    private double mutation_rate;
    private MutationRule mutation_rule;
    private CoordinatePicker location_picker;
    private List<Population> initial_populations;

    // World params
    private World world;
    private int rows;
    private int cols;
    private double migration_rate;
    private DilutionRule dil_rule;
    private MigrationRule migration_rule;
    private EnvironmentChanger env_changer;
    private StopCondition stop_condition;
    private String migration_range;
    private String migration_type;
    private String mutation_type;

    private String output_location;
    private File data_path;
    private int hours;
    private int iterations;
    private int save_every;
    private double save_every_dbl;

    // Subpopulation params
      
    // Common
    private boolean randomize;

    private double amount_for_new_cell;
    private double coop_release_rate;
    private double cheat_release_rate = 0.0;

    private double base_growth_rate = 0.45;
    private double base_death_rate = 0.1;
    private double base_km;
    
    // Cheater advantage.
    private double alpha;

    // Evolved Km advantage.
    private double delta1;

    // Evolved death advantage
    private double delta2;

    // Evolved tradeoff.
    private double theta;

    /** 
     * The constructor handles command line arguments and constructs an
     * <code>MutationAR</code> object, which is used to initialize
     * the populations.
     *
     */
    public MutationAR(String args[]) {
        parseArgs(args);
        setRNGs();
        scaleParams();
    }

    protected abstract DilutionRule makeDilutionRule();

    // Public methods
    public void run() {

        dil_rule = makeDilutionRule();

        mutation_rule = new MutateAdaptive(mutation_rate, coop_to_cheat_mutation_rate, cheat_to_coop_mutation_rate, mutation_rng);

        migration_rule = pickMigration();

        env_changer = new StaticEnvironment();
        location_picker = new UniqueRandomPicker(rows, cols, location_rng);

        if (initial_coop_freq < 1.0 && initial_coop_freq > 0.0 && coop_to_cheat_mutation_rate == 0) {
            stop_condition = new CoopCheatExtinctStop();
        } else {
            stop_condition = new CoopExtinctStop();
        }


        checkNulls();
        setDataPath();
        generatePopulations();
        initializeAndRunWorld();
    }

    private void initializeAndRunWorld() {
        StringBuffer arg_string = new StringBuffer();
        for (String arg:args) {
            arg_string.append(arg).append(" ");
        }
        System.out.println("Called with args\n" + 
                           getClass().getSimpleName() + " " +
                           arg_string.toString() + "\n");
        System.out.println(
            String.format("%n%nInitiating world with %s, %s migration, %.2e mutation rate and " +
                          "mutant frequency of %.2e",
                          migration_range,
                          migration_type,
                          mutation_rate, 
                          mutant_freq));
        System.out.println("\nSaving to " + data_path + "\n");


        world = new World(rows, cols, 
                          initial_populations,
                          env_changer,
                          dil_rule,
                          mutation_rule,
                          migration_rule,
                          stop_condition,
                          data_path,
                          TIMESTEP_SCALE);

        world.iterate(iterations, save_every);
    }

    private void generatePopulations() {
        if (mutant_freq > 1 || mutant_freq < 0) {
            throw new IllegalArgumentException("mutant freq (" + mutant_freq +
                                               ") out of range.");
        }
        if (alpha < 1) {
            throw new IllegalArgumentException("alpha (" + alpha + ") < 1.");
        }
        if (delta1 < 1) {
            throw new IllegalArgumentException("delta1 (" + delta1 + ") < 1.");
        }
        if (delta2 < 1) {
            throw new IllegalArgumentException("delta2 (" + delta2 + ") < 1.");
        }

        if (theta > 1 || theta == 0) {
            throw new IllegalArgumentException("theta (" + theta +
                                               ") out of range.");
        }


        // Ancestor cooperator.
        final String anc_coop_id = 
            "coop_" + String.format("%.3g", base_growth_rate) + "_" + String.format("%.3g", base_km);
        double co_vmax = base_growth_rate;
        double co_km   = base_km;
        double co_d    = base_death_rate;
        FitnessCalculator anc_coop_fc = 
            new MonodCalculator(co_vmax, co_km, co_d, TIMESTEP_SCALE);

        // Ancestor cheater.
        final String anc_cheat_id = 
            "cheat_" + String.format("%.3g", alpha*base_growth_rate) + "_" + String.format("%.3g", base_km);
        double ch_vmax = alpha*co_vmax;
        double ch_km = co_km;
        double ch_d = co_d/alpha;
        FitnessCalculator anc_cheat_fc = 
            new MonodCalculator(ch_vmax, ch_km, ch_d, TIMESTEP_SCALE);

        // Evolved cooperator.
        final String evo_coop_id = 
            "coop_" + String.format("%.3g", theta*base_growth_rate) + "_" + String.format("%.3g", base_km/delta1);
        double eco_vmax = theta*co_vmax;
        double eco_km   = co_km/delta1;
        double eco_d    = co_d/delta2;
        FitnessCalculator evo_coop_fc = 
            new MonodCalculator(eco_vmax, eco_km, eco_d, TIMESTEP_SCALE);

        // Evolved cheater.
        final String evo_cheat_id = 
            "cheat_" + String.format("%.3g", alpha*theta*base_growth_rate) + "_" + String.format("%.3g", base_km/delta1);
        double ech_vmax = alpha*eco_vmax;
        double ech_km   = eco_km;
        double ech_d    = eco_d/alpha;
        FitnessCalculator evo_cheat_fc = 
            new MonodCalculator(ech_vmax, ech_km, ech_d, TIMESTEP_SCALE);

        initial_n_pops = rounded(frac_occupied*rows*cols);
        int initial_coops = rounded(initial_pop_size*initial_coop_freq);
        int initial_cheats = rounded(initial_pop_size*(1-initial_coop_freq));

        int initial_anc_coops = rounded(initial_coops*(1-mutant_freq));
        int initial_anc_cheats = rounded(initial_cheats*(1-mutant_freq));
        int initial_evo_coops = rounded(initial_coops*mutant_freq);
        int initial_evo_cheats = rounded(initial_cheats*mutant_freq);

        Subpopulation anc_coop = new Subpopulation(initial_anc_coops,
                                                   amount_for_new_cell,
                                                   coop_release_rate,
                                                   anc_coop_fc,
                                                   anc_coop_id,
                                                   population_rng);

        Subpopulation anc_cheat = new Subpopulation(initial_anc_cheats,
                                                    amount_for_new_cell,
                                                    cheat_release_rate,
                                                    anc_cheat_fc,
                                                    anc_cheat_id,
                                                    population_rng);

        Subpopulation evo_coop = new Subpopulation(initial_evo_coops,
                                                   amount_for_new_cell,
                                                   coop_release_rate,
                                                   evo_coop_fc,
                                                   evo_coop_id,
                                                   population_rng);

        Subpopulation evo_cheat = new Subpopulation(initial_evo_cheats,
                                                    amount_for_new_cell,
                                                    cheat_release_rate,
                                                    evo_cheat_fc,
                                                    evo_cheat_id,
                                                    population_rng);

        Subpopulation empty_anc_coop = new Subpopulation(anc_coop);
        Subpopulation empty_anc_cheat = new Subpopulation(anc_cheat);
        Subpopulation empty_evo_coop = new Subpopulation(evo_coop);
        Subpopulation empty_evo_cheat = new Subpopulation(evo_cheat);
        empty_anc_coop.setSize(0);
        empty_anc_cheat.setSize(0);
        empty_evo_coop.setSize(0);
        empty_evo_cheat.setSize(0);

        List<Subpopulation> initial_subpopulations =
            new ArrayList<Subpopulation>(
                Arrays.asList(anc_coop, anc_cheat, evo_coop, evo_cheat));

        List<Subpopulation> empty_subpopulations =
            new ArrayList<Subpopulation>(
                Arrays.asList(empty_anc_coop, empty_anc_cheat,
                              empty_evo_coop, empty_evo_cheat));

        if (randomize) {
            initial_populations = new ArrayList<Population>(
                    Population.generate(initial_n_pops,
                                        initial_subpopulations,
                                        initial_resource,
                                        location_picker,
                                        population_rng));
        } else {
        // To generate populations with identical sizes.
            initial_populations = new ArrayList<Population>(initial_n_pops);
            for (int i=0; i<initial_n_pops; i++) {
                initial_populations.add(
                        new Population(initial_subpopulations,
                                       location_picker.pick(),
                                       initial_resource,
                                       population_rng));
            }
        }

        List<Coordinate> occupied_coords = 
            new ArrayList<Coordinate>(rows*cols);
        for (Population pop:initial_populations) {
            occupied_coords.add(pop.getCoordinate());
        }

        // Fill in empty locations
        for (int r=1; r<rows+1; r++) {
            for (int c=1; c<cols+1; c++) {
                Coordinate coord = new Coordinate(r,c);
                if (!occupied_coords.contains(coord)) {
                    initial_populations.add(
                        new Population(empty_subpopulations,
                                       coord,
                                       initial_resource,
                                       population_rng));
                }
            }
        }
    }

    private int rounded(double val) {
        return (int) StrictMath.round(val);
    }

    private void checkNulls() {
        if (this.dil_rule == null) {
            throw new RuntimeException("DilutionRule is null!\n");
        }

        if (this.env_changer == null) {
            throw new RuntimeException("EnvironmentChanger is null!\n");
        }

        if (this.location_picker == null) {
            throw new RuntimeException("Location piker is null!\n");
        }

        if (this.mutation_rule == null) {
            throw new RuntimeException("MutationRule is null!\n");
        }

        if (this.migration_rule == null) {
            throw new RuntimeException("MigrationRule is null!\n");
        }
    }

    private void setRNGs() {
        population_rng = new RandomNumberUser(population_seed);
        location_rng   = new RandomNumberUser(location_seed);
        migration_rng  = new RandomNumberUser(migration_seed);
        mutation_rng   = new RandomNumberUser(mutation_seed);
        //env_change_rng = new RandomNumberUser(env_change_seed);
    }

    private void parseArgs(String args[]) {
        this.args = args;
        int expected_length = 27;

        if (args.length < expected_length) {
           // for (String arg:args) {
           //    System.out.println(arg);
           // }

            throw new IllegalArgumentException(
            "usage: TestMutationAR " +
            "[migration range] " +
            "[migration type] " +
            "[initial pop size]" +
            "[mutant freq] " + 
            "[coop_release] " +
            "[amount_needed] " +
            "[coop freq] " +
            "[base Km] " +
            "[cheat adv] " +
            "[evolved km advantage] " +
            "[evolved death advantage] " +
            "[evolved vmax tradeoff] " +
            "[initial resource] " +
            "[row/col size] [frac occupied] [migration rate] " +
            "[coop to cheat mutation]" +
            "[cheat to coop mutation]" +
            "[mutation rate]" +
            "[randomize]" +
            "[population seed] " +
            "[location seed] " +
            "[migration seed]" +
            "[mutation seed]" +
            "[env change seed] " +
            "[hours to simulate] [save frequency (hrs)] [output_location]" +
            "\n\n");

        }

        migration_range = args[0];
        migration_type = args[1];
        mutation_type = args[2];
        initial_pop_size = (int) Double.parseDouble(args[3]);
        mutant_freq = Double.parseDouble(args[4]);
        coop_release_rate = Double.parseDouble(args[5]);
        amount_for_new_cell = Double.parseDouble(args[6]);
        initial_coop_freq = Double.parseDouble(args[7]);
        base_km = Double.parseDouble(args[8]);
        alpha = Double.parseDouble(args[9]);
        delta1 = Double.parseDouble(args[10]);
        delta2 = Double.parseDouble(args[11]);
        theta = Double.parseDouble(args[12]);
        initial_resource = Double.parseDouble(args[13]);
        rows = (int) Double.parseDouble(args[14]);
        cols = rows;
        frac_occupied = Double.parseDouble(args[15]);
        migration_rate = Double.parseDouble(args[16]);
        coop_to_cheat_mutation_rate = Double.parseDouble(args[17]);
        cheat_to_coop_mutation_rate = Double.parseDouble(args[18]);
        mutation_rate = Double.parseDouble(args[19]);
        randomize = Boolean.parseBoolean(args[20]);
        population_seed = Long.parseLong(args[21]);
        location_seed = Long.parseLong(args[22]);
        migration_seed = Long.parseLong(args[23]);
        mutation_seed = Long.parseLong(args[24]);
        env_change_seed = Long.parseLong(args[25]);
        hours = (int) Double.parseDouble(args[26]);
        save_every_dbl = Double.parseDouble(args[27]);
        if (args.length == expected_length+1) {
           output_location = args[expected_length];           
        } else {
            output_location = args[expected_length+1];
        }

    }

    private void scaleParams() {
        coop_to_cheat_mutation_rate /= TIMESTEP_SCALE;
        cheat_to_coop_mutation_rate /= TIMESTEP_SCALE;
        mutation_rate /= TIMESTEP_SCALE;
        coop_release_rate /= TIMESTEP_SCALE;
        migration_rate /= TIMESTEP_SCALE;
        iterations = rounded(hours*TIMESTEP_SCALE);
        save_every = rounded(save_every_dbl*TIMESTEP_SCALE);
    }

    private MigrationRule pickMigration() {
        boolean exclude_current = true;
        int max_distance = 1;
        CoordinatePicker picker;

        if (migration_range.equals("global")) {
            picker = new RandomPicker(rows, cols, exclude_current,
                                      migration_rng);
        } else if (migration_range.equals("local")) {
            picker = new RandomNeighborPicker(rows, cols, exclude_current,
                                              max_distance, migration_rng);
        } else {
            throw new IllegalArgumentException(
                    "Don't recognize that migration range!");
        }

        if (migration_type.equals("indv")) {
            return new IndividualMigration(migration_rate, picker);
        } else if (migration_type.equals("prop")) {
            return new PropaguleMigration(migration_rate, picker);
        } else {
            throw new IllegalArgumentException(
                    "Don't recognize that migration type!");
        }
    }

    private void setDataPath() {
        data_path = new File(output_location, UUID.randomUUID().toString());
        System.out.println(output_location);
        System.out.println(data_path);
    }

    public World getWorld() { return world; }
}
