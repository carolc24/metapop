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

package org.fhcrc.honeycomb.metapop.dilution;

import org.fhcrc.honeycomb.metapop.Population;
import org.fhcrc.honeycomb.metapop.StepProvider;
import org.fhcrc.honeycomb.metapop.coordinate.Coordinate;

import java.util.List;
import java.util.Map;

/** 
 * Dilutes a world.
 *
 * Created on 26 Apr, 2013
 *
 * @author Adam Waite
 * @version $Rev: 2393 $, $Date: 2014-05-24 19:17:59 -0400 (Sat, 24 May 2014) $, $Author: ajwaite $
 */
public interface DilutionRule {
    /**
     * Given a list of {@code Population}s, returns a map describing 
     * the amount by which each {@code Coordinate} should be diluted.
     *
     * @param pops the list of {@link Population}s to be diluted.
     *
     * @return a map specifying which {@link Coordinate}s should be diluted.
     */
    public Map<Coordinate, Double> generate(List<Population> pops);

    public void setStepProvider(StepProvider step_provider);
}
