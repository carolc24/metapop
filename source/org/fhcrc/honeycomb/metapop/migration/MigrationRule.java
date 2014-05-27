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

package org.fhcrc.honeycomb.metapop.migration;

import org.fhcrc.honeycomb.metapop.Population;
import org.fhcrc.honeycomb.metapop.OccupiedLocations;
import org.fhcrc.honeycomb.metapop.coordinate.Coordinate;
import org.fhcrc.honeycomb.metapop.coordinate.CoordinateProvider;
import org.fhcrc.honeycomb.metapop.coordinate.picker.CoordinatePicker;

import java.util.List;

/** 
 * Controls migration
 *
 * Created on 30 May, 2013
 *
 * @author Adam Waite
 * @version $Rev: 2393 $, $Date: 2014-05-24 19:17:59 -0400 (Sat, 24 May 2014) $, $Author: ajwaite $
 *
 */
public abstract class MigrationRule implements CoordinateProvider {
    private double rate;
    private CoordinatePicker picker;
    private Coordinate coordinate;

    protected MigrationRule() {}

    public MigrationRule(double rate, CoordinatePicker picker) {
        this.rate = rate;
        this.picker = picker;
        picker.setProvider(this);
    }

    public abstract void migrate(OccupiedLocations ols);

    public double getRate() { return rate; }
    public CoordinatePicker getPicker() { return picker; }
    public void setCoordinate(Coordinate coord) {
        this.coordinate = coord;
    }


    @Override
    public Coordinate getCoordinate() { return coordinate; }

    @Override
    public String toString() {
        return String.format("%s, rate=%s, picker=%s",
                             this.getClass().getSimpleName(), rate, picker);
    }
}
