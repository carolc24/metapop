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

package org.fhcrc.honeycomb.metapop;

import java.util.List;
import org.apache.commons.math3.random.RandomDataGenerator;
import org.apache.commons.math3.random.RandomGenerator;
import org.apache.commons.math3.random.Well19937c;

/** 
 * A wrapper class for all random number generation.
 *
 * Created on 12 Feb, 2012
 * @author Adam Waite
 * @version $Rev: 2393 $, $Date: 2014-05-24 19:17:59 -0400 (Sat, 24 May 2014) $, $Author: ajwaite $
 *
 */
public class RandomNumberUser {
    protected final long DEFAULT_SEED = System.nanoTime();

    protected RandomDataGenerator rng = null;
    protected long seed;

    public RandomNumberUser(long seed) {
        this.seed = seed;
        this.rng = new RandomDataGenerator(new Well19937c(seed));
    }

    public RandomNumberUser() {
        this(System.nanoTime());
    }

    public int getNextInt(int min, int max) {
        return rng.nextInt(min, max);
    }

    public double getNextDouble(double min, double max) {
        return rng.nextUniform(min, max);
    }

    public int getNextBinomial(int n, double p) {
        return rng.nextBinomial(n, p);
    }

    public int getNextBinomial(int n, Double p) {
        return rng.nextBinomial(n, p.doubleValue());
    }

    public long getNextPoisson(double m) {
        return (m==0) ? 0 : rng.nextPoisson(m);
    }

    public int[] getNextPermutation(int n, int k) {
        return rng.nextPermutation(n, k);
    }

    public void reSeed(long seed) { 
        this.seed = seed;
        rng.reSeed(seed);
    }

    public long getSeed() { return seed; }
    public RandomDataGenerator getRNG() { return rng; }
}
