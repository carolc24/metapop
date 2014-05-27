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

import java.io.File;
import java.util.Map;
import java.util.List;

/**
 * Saves the state of {@code Saveable} objects.
 * Created on 28 Apr, 2013.
 * @author Adam Waite
 * @version $Rev: 2393 $, $Date: 2014-05-24 19:17:59 -0400 (Sat, 24 May 2014) $, $Author: ajwaite $
 */
public interface Saveable {

    /** the save path */
    public File getDataPath();

    /** the filename */
    public String getFilename();

    /** the headers.  The order is how the data should be written.*/
    public String getHeaders();

    /** 
     * how this {@code Saveable} was set up. Should have entry for 'filename'
     * and one for 'info'.
     * */
    public Map<String, String> getInitializationData();

    /** data information. */
    public String getData();
}
