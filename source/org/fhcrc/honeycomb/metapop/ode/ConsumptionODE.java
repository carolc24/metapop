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

package org.fhcrc.honeycomb.metapop.ode;

import org.fhcrc.honeycomb.metapop.Population;
import org.fhcrc.honeycomb.metapop.Subpopulation;
import org.fhcrc.honeycomb.metapop.fitness.FitnessCalculator;
import org.fhcrc.honeycomb.metapop.coordinate.Coordinate;

import org.apache.commons.math3.ode.FirstOrderDifferentialEquations;
import org.apache.commons.math3.ode.FirstOrderIntegrator;
import org.apache.commons.math3.ode.nonstiff.DormandPrince54Integrator;
import org.apache.commons.math3.ode.sampling.FixedStepHandler;
import org.apache.commons.math3.ode.sampling.StepHandler;
import org.apache.commons.math3.ode.sampling.StepNormalizer;
import org.apache.commons.math3.ode.sampling.StepInterpolator;
import org.apache.commons.math3.ode.sampling.StepNormalizerMode;
import org.apache.commons.math3.ode.sampling.StepNormalizerBounds;
import org.apache.commons.math3.ode.events.EventHandler;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;

/** 
 * Uses ODEs to accurately calculate growth rates.
 *
 * Created on 2 Aug, 2013
 * @author Adam Waite
 * @version $Rev: 2393 $, $Date: 2014-05-24 19:17:59 -0400 (Sat, 24 May 2014) $, $Author: ajwaite $
 *
 */
public class ConsumptionODE implements FirstOrderDifferentialEquations {
    private Population pop;
    private List<Subpopulation> subpops = new ArrayList<Subpopulation>();
    private int n_subpops;
    private int n_states;
    private double capacity;

    // Integrator
    private double minStep = 1.0e-12;
    private double maxStep = 1;
    private double scalAbsoluteTolerance = 1.0e-10;
    private double scalRelativeTolerance = 1.0e-7;

    private FirstOrderIntegrator integrator = 
        new DormandPrince54Integrator(minStep, maxStep,
                                      scalAbsoluteTolerance,
                                      scalRelativeTolerance);

    private double initial_time = 0.0;
    private double timestep_length;
    private double[] init;
    private double[] result;
    private Map<String, Double> result_map = new HashMap<String, Double>();
    private Map<String, Double> init_map = new HashMap<String, Double>();

    // For integration of resource
    private int steps = 1000;
    private double step_size = 1.0/steps;
    private int step_idx = 0;
    private double[] resource_array = new double[steps+1];


    private FixedStepHandler fixedHandler;
    private StepHandler normalizer;

    public ConsumptionODE() {}

    public ConsumptionODE(Population pop) {
        this(pop, 1);
    }

    public ConsumptionODE(Population pop, double timestep_length) {
        this.pop = pop;
        this.capacity = pop.getCapacity();
        this.subpops = pop.getSubpopulations();
        this.n_subpops = this.subpops.size();
        this.n_states = n_subpops + 1;
        this.timestep_length = timestep_length;

        init = new double[n_states];

        fixedHandler = new FixedStepHandler() {
            @Override
            public void init(double t0, double[] y0, double t) {
                step_idx = 0;
            }

            @Override
            public void handleStep(double t, double[] y, double[] yDot,
                                   boolean isLast)
            {
                resource_array[step_idx++] = y[n_subpops];
            }
        };
        //System.out.println("step size: " + timestep_length/steps);
        normalizer = new StepNormalizer(timestep_length * step_size,
                                        fixedHandler,
                                        StepNormalizerBounds.BOTH);

        integrator.addStepHandler(normalizer);
    }

    @Override
    public int getDimension() { return n_states; }

    @Override
    public void computeDerivatives(double t, double[] y, double[] yDot) {
        double total_size = 0.0;
        for (int i=0; i<n_subpops; i++) { total_size += y[i]; }

        double S = y[y.length-1];
        double S_new = 0.0;
        double gr = 0.0;
        double dr = 0.0;
        double release = 0.0;
        double consumption = 0.0;
        FitnessCalculator fc;
        Subpopulation subpop;
        double scale = 1-(total_size/capacity);
        for (int i=0; i<n_subpops; i++) {
            subpop = subpops.get(i);
            fc = subpop.getFitnessCalculator();
            gr = fc.calculateGrowthRate(S);
            dr = fc.calculateDeathRate(S);

            // dN/dt
            yDot[i] = y[i]*(gr-dr);

            S_new += y[i]*(subpop.getReleaseRate()*scale -
                           subpop.getGamma()*gr);
        }
        // dS/dt
        yDot[yDot.length-1] = S_new;
    }

    public void integrate(Population pop) {
        this.pop = pop;

        for (int i=0; i<n_subpops; i++) { 
            Subpopulation subpop = pop.getSubpopulations().get(i);
            init[i] = subpop.getSize();
        }
        init[n_subpops] = pop.getResource();
        result = Arrays.copyOf(init, init.length);

        integrator.integrate(this, initial_time, result, timestep_length,
                             result); 
    }

    public void makeResults() {
        init_map = new HashMap<String, Double>();
        result_map = new HashMap<String, Double>();
        for (int i=0; i<n_subpops; i++) {
            init_map.put(subpops.get(i).getId(), init[i]);
            result_map.put(subpops.get(i).getId(), result[i]);
        }
        init_map.put("resource", init[n_subpops]);
        result_map.put("resource", result[n_subpops]);
    }

    public Map<String,Double> getResult() {
        makeResults();
        return result_map;
    }

    public void reportResults() {
        makeResults();
        System.out.println("init: " + init_map.toString());
        System.out.println("final: " + result_map.toString());
        System.out.println("timestep length: " + timestep_length);
    }

    public double calculateGrowthRate(String id) {
        // dN/dt = rN
        if (result_map.size() == 0) makeResults();
        double N = result_map.get(id);
        double dN_dt = (N-init_map.get(id))/timestep_length;
        return dN_dt/N;
    }

    public double integrateResource() {
        double sum = 0.0;
        for (int i=1; i<steps; i++) { 
            sum += resource_array[i];
        }
        sum += 0.5*(resource_array[0]+resource_array[steps]);
        return sum*step_size;
    }

    public double getFinalResource() {
        return result[n_subpops];
    }

    public double getInitialResource() {
        return init[n_subpops];
    }

    public void printResourceArray() {
        for (int i=0; i<steps; i++) {
            System.out.println("resource_array[" + i + "]: " + 
                               resource_array[i]);
        }
    }

    @Override
    public String toString() {
        return String.format("integrator=%s, minStep=%.2e, maxStep=%.2e, " +
                             "AbsTol=%.2e, RelTol=%.2e",
                             integrator.getClass().getSimpleName(),
                             minStep, maxStep, scalAbsoluteTolerance,
                             scalRelativeTolerance);
    }
}
