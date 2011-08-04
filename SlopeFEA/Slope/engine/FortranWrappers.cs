/************************************************************************
 * PROJECT:     SlopeFEA (c) 2011 Brandon Karchewski
 *              Licensed under the Academic Free License version 3.0
 *                  http://www.opensource.org/licenses/afl-3.0.php
 * 
 * CONTACT:     Brandon Karchewski
 *              Department of Civil Engineering
 *              McMaster University, JHE-301
 *              1280 Main St W
 *              Hamilton, Ontario, Canada
 *              L8S 4L7
 *              p: 905-525-9140 x24287
 *              f: 905-529-9688
 *              e: karcheba@mcmaster.ca
 *              
 * 
 * SOURCE INFORMATION:
 * 
 * The repository for this software project is hosted on git at:
 *      
 *      git://github.com/karcheba/SlopeFEA
 *      
 * As such, the code for the project is free and open source.
 * The relevant license is AFLv3 (see link above). See the
 * README file in the root directory of the repository for a
 * detailed project description, acknowledgements, references,
 * and the revision history.
 ************************************************************************/

using System.Runtime.InteropServices;

namespace SlopeFEA
{
    public class FortranWrappers
    {
        [DllImport( "DLLtest.dll" , CallingConvention = CallingConvention.Cdecl )]
        public static extern void dlltest_ ( string fname , int strLen );

        [DllImport( "SlopeFEA3NodeMultiPhase.dll" , CallingConvention = CallingConvention.Cdecl )]
        public static extern void slopefea3node_ ( string fpath , int len );

        [DllImport( "SlopeFEA4Node.dll" , CallingConvention = CallingConvention.Cdecl )]
        public static extern void slopefea4node_ ( string fpath , int len );
    }
}