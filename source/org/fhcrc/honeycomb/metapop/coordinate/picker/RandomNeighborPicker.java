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

package org.fhcrc.honeycomb.metapop.coordinate.picker;

import org.fhcrc.honeycomb.metapop.RandomNumberUser;
import org.fhcrc.honeycomb.metapop.coordinate.Coordinate;
import org.fhcrc.honeycomb.metapop.coordinate.CoordinateProvider;

import java.util.List;
import java.util.ArrayList;
import java.util.Set;
import java.util.HashSet;

/** 
 * Generates random <code>Coordinates</code>s a specified distance from the
 * given coordinate.
 *
 * Created on 8 Feb, 2012
 * @author Adam Waite
 * @version $Rev: 2393 $, $Date: 2014-05-24 19:17:59 -0400 (Sat, 24 May 2014) $, $Author: ajwaite $
 *
 */
public class RandomNeighborPicker extends CoordinatePicker {
    protected final int max_distance;

    public RandomNeighborPicker(int max_row, int max_col,
                                boolean exclude_current,
                                CoordinateProvider provider,
                                int max_distance, RandomNumberUser rng)
    {
        super(max_row, max_col, exclude_current, provider, rng);
        this.max_distance = max_distance;
    }

    public RandomNeighborPicker(int max_row, int max_col, 
                                boolean exclude_current, int max_distance,
                                RandomNumberUser rng)
    {
        this(max_row, max_col, exclude_current, null, max_distance, rng);
    }

    @Override
    public List<Coordinate> pick(int n) {
        List<Coordinate> coordinates = new ArrayList<Coordinate>(n);

        if (provider.getCoordinate() == null) {
            throw new NullPointerException("Current location not set");
        }

        Coordinate current = provider.getCoordinate();
        int current_row = current.getRow();
        int current_col = current.getCol();

        int new_row, new_col;
        while (coordinates.size() < n) {
            new_row = getNewSpot(current_row, max_row);
            new_col = getNewSpot(current_col, max_col);
            if (exclude_current == true) {
                while (new_row == current_row && new_col == current_col) {
                    new_row = getNewSpot(current_row, max_row);
                    new_col = getNewSpot(current_col, max_col);
                }
            }
            coordinates.add(new Coordinate(new_row, new_col));
        }
        return coordinates;
    }

    @Override
    public Coordinate pick() { return pick(1).get(0); }

    public int getNewSpot(int current, int limit) {
        int move = rng.getNextInt(0,max_distance*2)-max_distance;

        int new_spot = move+current;
        //System.out.println("move :"+move);
        //System.out.println("current: " + current);
        //System.out.println("new spot: " + new_spot);

        if (new_spot > limit) {
            new_spot = new_spot-limit;
        } else if (new_spot < 1) {
            new_spot = limit+new_spot;
        }
        //System.out.println("new spot adj: " + new_spot);
        return new_spot;
    }

    public int getMaxDistance() { return max_distance; }

    public String getType() {
        return "local_dist=" + max_distance;
    }

    @Override
    public String toString() {
        return super.toString() + ", max_dist=" + max_distance;
    }
}
