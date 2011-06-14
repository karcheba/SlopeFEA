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

using System;
using System.IO;
using System.Windows;

namespace Matrix
{
    class DenseMatrix
    {
        private double[,] data;
        private int nrows , ncols;

        private static Random rand = new Random();

        public DenseMatrix ( int nrows = 1 , int ncols = 1 , bool identity = false )
        {
            this.nrows = nrows;
            this.ncols = ncols;
            data = new double[nrows , ncols];

            if ( identity ) FillIdentity();
        }

        public int NumRows { get { return this.nrows; } }
        public int NumCols { get { return this.ncols; } }

        public bool IsSquare { get { return nrows == ncols; } }
        public bool IsSymmetric { get { return this == this.Transpose; } }

        /// <summary>
        /// Indexer for getting or setting a single entry
        /// </summary>
        /// <param name="i">The zero-based row index</param>
        /// <param name="j">The zero-based column index</param>
        /// <returns>The double precision floating point value located at index i,j</returns>
        public double this[int i , int j , bool checkindex = false]
        {
            get
            {
                if ( checkindex ) if ( !this.checkIndex( i , j ) ) throw new IndexOutOfRangeException();

                return data[i , j];
            }

            set
            {
                if ( checkindex ) if ( !this.checkIndex( i , j ) ) throw new IndexOutOfRangeException();

                data[i , j] = value;
            }
        }

        /// <summary>
        /// Indexer for getting an entire row or column
        /// </summary>
        /// <param name="i">The zero-based index of the row or column</param>
        /// <param name="getcol">Boolean for getting a column (default it false, meaning return a row)</param>
        /// <returns>A 1xNumCols or NumRowsx1 DenseMatrix containing the row or column called.</returns>
        public DenseMatrix this[int i , bool getcol = false]
        {
            get
            {
                DenseMatrix ans = null;
                if ( getcol )
                {
                    ans = new DenseMatrix( nrows , 1 );
                    for ( int j = 0 ; j < nrows ; j++ ) ans[j , 0] = data[j , i];
                }
                else
                {
                    ans = new DenseMatrix( 1 , ncols );
                    for ( int j = 0 ; j < ncols ; j++ ) ans[0 , j] = data[i , j];
                }

                return ans;
            }
        }

        private bool checkIndex ( int i , int j )
        {
            return !(i >= nrows || j >= ncols || i < 0 || j < 0);
        }

        public bool IsPosDef
        {
            get
            {
                // only square matrices can possibly be positive definite
                if ( !IsSquare ) return false;

                // check if determinant of all minors are strictly positive
                DenseMatrix[] lu = this.LU;
                for ( int i = 0 ; i < nrows ; i++ )
                    if ( lu[1][i , i] <= 0 ) return false;

                return true;
            }
        }

        public DenseMatrix Transpose
        {
            get
            {
                DenseMatrix ans = new DenseMatrix( ncols , nrows );
                for ( int i = 0 ; i < ncols ; i++ )
                    for ( int j = 0 ; j < nrows ; j++ )
                        ans[i , j] = this[j , i];

                return ans;
            }
        }

        public double Determinant
        {
            get
            {
                if ( !IsSquare ) throw new InvalidOperationException( "Can only take determinant of square matrix." );

                // scalar
                if ( nrows == 1 ) return this[0 , 0];

                // 2x2 matrix
                else if ( nrows == 2 ) return this[0 , 0] * this[1 , 1] - this[0 , 1] * this[1 , 0];

                // nxn using LU decomposition
                else
                {
                    DenseMatrix[] lu = this.LU;
                    double ans = 1.0;
                    for ( int i = 0 ; i < nrows ; i++ )
                        ans *= lu[1][i , i];

                    return ans;
                }
            }
        }

        public DenseMatrix[] LU
        {
            get
            {
                // create L and U matrices for this=[L][U] decomposition
                DenseMatrix L = new DenseMatrix( nrows , ncols , /* identity = */ true );
                DenseMatrix U = this.Copy();

                // loop through rows
                for ( int k = 0 ; k < nrows ; k++ )
                {
                    // loop through lower rows
                    for ( int i = k + 1 ; i < nrows ; i++ )
                    {
                        L[i , k] = U[i , k] / U[k , k];
                        U[i , k] = 0;

                        // loop through columns
                        for ( int j = k + 1 ; j < ncols ; j++ )
                            U[i , j] -= L[i , k] * U[k , j];
                    }
                }

                return new DenseMatrix[] { L , U };
            }
        }


        public DenseMatrix[] Cholesky
        {
            get
            {
                try
                {
                    // L matrix for this=[L][D][L]^T decomposition
                    DenseMatrix L = new DenseMatrix( nrows , ncols , /* identity = */ true );
                    DenseMatrix D = new DenseMatrix( nrows , ncols );

                    // loop through rows
                    double offdiagsum , diagsum;
                    for ( int k = 0 ; k < nrows ; k++ )
                    {
                        // loop through off-diagonal columns
                        for ( int i = 0 ; i < k ; i++ )
                        {
                            offdiagsum = 0;
                            for ( int j = 0 ; j < i ; j++ )
                                offdiagsum += L[i , j] * L[k , j];

                            L[k , i] = (this[k , i] - offdiagsum) / D[i , i];
                        }

                        // diagonal entries
                        diagsum = 0;
                        for ( int j = 0 ; j < k ; j++ )
                            diagsum += L[k , j] * L[k , j];

                        D[k , k] = this[k , k] - diagsum;

                        if ( D[k , k] <= 0 )
                            throw new InvalidOperationException( "Error! Cannot obtain Cholesky decomposition because matrix is not positive definite." );
                    }

                    return new DenseMatrix[] { L , D };
                }
                catch ( InvalidOperationException e )
                {
                    MessageBox.Show( e.Message , "Error" );
                    return null;
                }
            }
        }

        public DenseMatrix Inverse
        {
            get
            {
                if ( !IsSquare || Math.Abs( Determinant ) < 1e-5 ) throw new InvalidOperationException( "Only square non-singular matrices have an inverse." );

                /* Note: One might think there is an opportunity to optimize this operation
                 *          by checking if the matrix is symmetric and positive definite, in which
                 *          case Cholesky decomposition would be faster. However, since determining
                 *          if the matrix is positive definite requires that all matrix minors
                 *          be strictly positive (i.e. det>0) and the computation of the determinant
                 *          uses LU decomposition (since it is faster), it is actually quicker to
                 *          just use LU decomposition once here.
                 */
                DenseMatrix[] lu = this.LU;

                // initialize inverse with appropriate solution vectors in columns
                DenseMatrix inv = new DenseMatrix( nrows , ncols , /* identity = */ true );

                // loop through column solution vectors
                for ( int k = 0 ; k < ncols ; k++ )
                {
                    // forward substitution
                    inv[0 , k] /= lu[0][0 , 0];
                    for ( int i = 1 ; i < nrows ; i++ )
                    {
                        for ( int j = 0 ; j < i ; j++ )
                            inv[i , k] -= lu[0][i , j] * inv[j , k];

                        inv[i , k] /= lu[0][i , i];
                    }

                    // back substitution
                    inv[nrows - 1 , k] /= lu[1][nrows - 1 , nrows - 1];
                    for ( int i = nrows - 2 ; i >= 0 ; i-- )
                    {
                        for ( int j = i + 1 ; j < ncols ; j++ )
                            inv[i , k] -= lu[1][i , j] * inv[j , k];

                        inv[i , k] /= lu[1][i , i];
                    }
                }

                return inv;
            }
        }

        public void Print ( TextWriter tw )
        {
            for ( int i = 0 ; i < nrows ; i++ )
            {
                tw.Write( "[\t" );
                for ( int j = 0 ; j < ncols ; j++ ) tw.Write( String.Format( "{0:e6}\t" , this[i , j] ) );
                tw.WriteLine( "]" );
            }
        }

        public void PrintToFile ( string path , bool append = false )
        {
            using ( TextWriter tw = append ? File.AppendText( path ) : new StreamWriter( path ) )
            {
                for ( int i = 0 ; i < nrows ; i++ )
                {
                    for ( int j = 0 ; j < ncols ; j++ ) tw.Write( String.Format( "{0:e6}\t" , this[i , j] ) );
                    tw.WriteLine();
                }
            }
        }

        public override string ToString ()
        {
            string output = "";
            for ( int i = 0 ; i < nrows ; i++ )
            {
                output += String.Format( "[\t" );
                for ( int j = 0 ; j < ncols ; j++ ) output += String.Format( "{0:e6}\t" , this[i , j] );
                output += String.Format( "]\n" );
            }
            return output;
        }

        public DenseMatrix Copy ()
        {
            DenseMatrix copy = new DenseMatrix( nrows , ncols );
            for ( int i = 0 ; i < nrows ; i++ )
                for ( int j = 0 ; j < ncols ; j++ )
                    copy[i , j] = this[i , j];

            return copy;
        }

        public void CopyInPlace ( ref DenseMatrix copy )
        {
            if ( copy.NumRows != nrows || copy.NumCols != ncols )
                throw new ArgumentException( "Destination dims must match source dims." );

            for ( int i = 0 ; i < nrows ; i++ )
                for ( int j = 0 ; j < ncols ; j++ )
                    copy[i , j] = this[i , j];
        }

        public void FillZeros ()
        {
            for ( int i = 0 ; i < nrows ; i++ )
                for ( int j = 0 ; j < ncols ; j++ )
                    this[i , j] = 0.0;
        }

        public void FillRandom ( double start = 0 , double end = 10 )
        {
            for ( int i = 0 ; i < nrows ; i++ )
                for ( int j = 0 ; j < ncols ; j++ )
                    this[i , j] = start + (end - start) * rand.NextDouble();
        }

        public void FillIdentity ()
        {
            if ( !IsSquare ) throw new InvalidOperationException( "The matrix must be square to generate an identity." );

            for ( int i = 0 ; i < nrows ; i++ )
                for ( int j = 0 ; j < ncols ; j++ )
                    this[i , j] = (i == j) ? 1.0 : 0.0;
        }

        public void FillRandSymPosDef ( double start = 0 , double end = 10 )
        {
            if ( !IsSquare )
                throw new InvalidOperationException( "The matrix must be square to become symmetric positive definite." );

            // ensure all random values are non-negative
            if ( start < 0 || end < 0 )
                throw new ArgumentException( "Both start and end must be positive values." );

            // generate random spring stiffnesses
            double[] rands = new double[nrows];
            for ( int i = 0 ; i < nrows ; i++ ) rands[i] = start + (end - start) * rand.NextDouble();

            // build stiffness matrix for 1-D system of springs
            for ( int i = 0 ; i < nrows ; i++ )
            {
                for ( int j = 0 ; j < ncols ; j++ )
                {
                    if ( i == j )
                    {
                        this[i , j] = rands[i];
                        if ( j != NumCols - 1 ) this[i , j] += rands[i + 1];
                    }
                    else if ( i == j - 1 ) this[i , j] = -rands[j];
                    else if ( i == j + 1 ) this[i , j] = -rands[j + 1];
                    else this[i , j] = 0.0;
                }
            }
        }


        // ----------------------------------------------------------
        // OPERATORS
        // ----------------------------------------------------------

        public static explicit operator DenseMatrix ( BandSymMatrix m1 )
        {
            DenseMatrix ans = new DenseMatrix( m1.NumRows , m1.NumCols );
            for ( int i = 0 ; i < ans.NumRows ; i++ )
                for ( int j = 0 ; j < ans.NumCols ; j++ )
                    ans[i , j] = m1[i , j];

            return ans;
        }

        public override bool Equals ( object obj )
        {
            return base.Equals( obj );
        }

        public override int GetHashCode ()
        {
            return base.GetHashCode();
        }

        public static bool operator == ( DenseMatrix m1 , DenseMatrix m2 )
        {
            if ( m1.NumRows != m2.NumRows || m1.NumCols != m2.NumCols ) return false;

            double toler = 1e-8;
            for ( int i = 0 ; i < m1.NumRows ; i++ )
                for ( int j = 0 ; j < m1.NumCols ; j++ )
                    if ( Math.Abs( m1[i , j] - m2[i , j] ) > toler ) return false;

            return true;
        }

        public static bool operator != ( DenseMatrix m1 , DenseMatrix m2 )
        {
            return !(m1 == m2);
        }

        public static DenseMatrix operator + ( DenseMatrix m1 )
        {
            return m1.Copy();
        }

        public static DenseMatrix operator - ( DenseMatrix m1 )
        {
            DenseMatrix ans = new DenseMatrix( m1.NumRows , m1.NumCols );
            for ( int i = 0 ; i < ans.NumRows ; i++ )
                for ( int j = 0 ; j < ans.NumCols ; j++ )
                    ans[i , j] = -m1[i , j];

            return ans;
        }

        public static DenseMatrix operator + ( DenseMatrix m1 , DenseMatrix m2 )
        {
            if ( m1.NumRows != m2.NumRows || m1.NumCols != m2.NumCols )
                throw new ArgumentException( "Matrix dimensions must match to add them." );

            DenseMatrix ans = new DenseMatrix( m1.NumRows , m1.NumCols );
            for ( int i = 0 ; i < ans.NumRows ; i++ )
                for ( int j = 0 ; j < ans.NumCols ; j++ )
                    ans[i , j] = m1[i , j] + m2[i , j];

            return ans;
        }

        public static void AddInPlace ( DenseMatrix m1 , DenseMatrix m2 , ref DenseMatrix ans )
        {
            if ( m1.NumRows != m2.NumRows || m1.NumCols != m2.NumCols )
                throw new ArgumentException( "Matrix dimensions must match to add them." );

            if ( ans.NumRows != m1.NumRows || ans.NumCols != m1.NumCols )
                throw new ArgumentException( "Destination dims must match source dims." );

            for ( int i = 0 ; i < ans.NumRows ; i++ )
                for ( int j = 0 ; j < ans.NumCols ; j++ )
                    ans[i , j] = m1[i , j] + m2[i , j];
        }

        public static DenseMatrix operator + ( DenseMatrix m1 , double value )
        {
            DenseMatrix ans = new DenseMatrix( m1.NumRows , m1.NumCols );
            for ( int i = 0 ; i < ans.NumRows ; i++ )
                for ( int j = 0 ; j < ans.NumCols ; j++ )
                    ans[i , j] = m1[i , j] + value;

            return ans;
        }

        public static DenseMatrix operator + ( double value , DenseMatrix m1 )
        {
            return m1 + value;
        }

        public static DenseMatrix operator - ( DenseMatrix m1 , DenseMatrix m2 )
        {
            if ( m1.NumRows != m2.NumRows || m1.NumCols != m2.NumCols )
                throw new ArgumentException( "Matrix dimensions must match to add them." );

            DenseMatrix ans = new DenseMatrix( m1.NumRows , m1.NumCols );
            for ( int i = 0 ; i < ans.NumRows ; i++ )
                for ( int j = 0 ; j < ans.NumCols ; j++ )
                    ans[i , j] = m1[i , j] - m2[i , j];

            return ans;
        }

        public static DenseMatrix operator - ( DenseMatrix m1 , double value )
        {
            return m1 + (-value);
        }

        public static DenseMatrix operator - ( double value , DenseMatrix m1 )
        {
            return value + (-m1);
        }

        public static DenseMatrix operator * ( DenseMatrix m1 , DenseMatrix m2 )
        {
            if ( m1.NumCols != m2.NumRows )
                throw new InvalidOperationException( "Inner matrix dimensions must match to perform matrix multiplication." );

            DenseMatrix ans = new DenseMatrix( m1.NumRows , m2.NumCols );
            for ( int i = 0 ; i < ans.NumRows ; i++ )
                for ( int j = 0 ; j < ans.NumCols ; j++ )
                    for ( int k = 0 ; k < m2.NumRows ; k++ )
                        ans[i , j] += m1[i , k] * m2[k , j];

            return ans;
        }

        public static void MultiplyInPlace ( DenseMatrix m1 , DenseMatrix m2 , ref DenseMatrix ans )
        {
            if ( m1.NumCols != m2.NumRows )
                throw new InvalidOperationException( "Inner matrix dimensions must match to perform matrix multiplication." );

            if ( ans.NumRows != m1.NumRows || ans.NumCols != m2.NumCols )
                throw new ArgumentException( "Answer matrix dimensions must match input outer dimensions." );

            for ( int i = 0 ; i < ans.NumRows ; i++ )
                for ( int j = 0 ; j < ans.NumCols ; j++ )
                    for ( int k = 0 ; k < m2.NumRows ; k++ )
                        ans[i , j] += m1[i , k] * m2[k , j];
        }

        public static DenseMatrix operator * ( DenseMatrix m1 , double value )
        {
            DenseMatrix ans = new DenseMatrix( m1.NumRows , m1.NumCols );
            for ( int i = 0 ; i < ans.NumRows ; i++ )
                for ( int j = 0 ; j < ans.NumCols ; j++ )
                    ans[i , j] = m1[i , j] * value;

            return ans;
        }

        public static DenseMatrix operator * ( double value , DenseMatrix m1 )
        {
            return m1 * value;
        }

        public static void MultiplyInPlace ( double value , DenseMatrix m1 , ref DenseMatrix ans )
        {
            if ( ans.NumRows != m1.NumRows || ans.NumCols != m1.NumCols )
                throw new ArgumentException( "Destination dims must match source dims." );

            for ( int i = 0 ; i < ans.NumRows ; i++ )
                for ( int j = 0 ; j < ans.NumCols ; j++ )
                    ans[i , j] = m1[i , j] * value;
        }

        public static DenseMatrix ElementMult ( DenseMatrix m1 , DenseMatrix m2 )
        {
            if ( m1.NumRows != m2.NumRows || m1.NumCols != m2.NumCols )
                throw new InvalidOperationException( "Matrix dimensions must match to perform element-wise multiplication." );

            DenseMatrix ans = new DenseMatrix( m1.NumRows , m1.NumCols );
            for ( int i = 0 ; i < ans.NumRows ; i++ )
                for ( int j = 0 ; j < ans.NumCols ; j++ )
                    ans[i , j] = m1[i , j] * m2[i , j];

            return ans;
        }

        public static DenseMatrix operator / ( DenseMatrix m1 , DenseMatrix m2 )
        {
            return m2.Inverse * m1;
        }

        public static DenseMatrix operator / ( DenseMatrix m1 , double value )
        {
            return m1 * (1 / value);
        }

        public static void SolveInPlaceLU ( DenseMatrix[] lu , DenseMatrix b , ref DenseMatrix ans )
        {
            // forward substitution
            ans[0 , 0] = b[0 , 0];
            for ( int i = 1 ; i < ans.NumRows ; i++ )
            {
                ans[i , 0] = b[i , 0];
                for ( int j = 0 ; j < i ; j++ ) ans[i , 0] -= ans[j , 0] * lu[0][i , j];
            }

            // back substitution
            ans[ans.NumRows - 1 , 0] /= lu[1][ans.NumRows - 1 , ans.NumRows - 1];
            for ( int i = ans.NumRows - 2 ; i >= 0 ; i-- )
            {
                for ( int j = ans.NumRows - 1 ; j > i ; j-- ) ans[i , 0] -= ans[j , 0] * lu[1][i , j];
                ans[i , 0] /= lu[1][i , i];
            }
        }

        public static void SolveInPlaceCholesky ( DenseMatrix[] chol , DenseMatrix b , ref DenseMatrix ans )
        {
            // forward substitution
            ans[0 , 0] = b[0 , 0] / chol[1][0 , 0];
            for ( int i = 1 ; i < ans.NumRows ; i++ )
            {
                ans[i , 0] = b[i , 0];
                for ( int j = 0 ; j < i ; j++ ) ans[i , 0] -= ans[j , 0] * chol[0][i , j] * chol[1][j , j];
                ans[i , 0] /= chol[1][i , i];
            }

            // back substitution
            for ( int i = ans.NumRows - 2 ; i >= 0 ; i-- )
                for ( int j = ans.NumRows - 1 ; j > i ; j-- )
                    ans[i , 0] -= ans[j , 0] * chol[0][j , i];
        }

        public static DenseMatrix ElementDiv ( DenseMatrix m1 , DenseMatrix m2 )
        {
            if ( m1.NumRows != m2.NumRows || m1.NumCols != m2.NumCols )
                throw new InvalidOperationException( "Matrix dimensions must match to perform element-wise division." );

            DenseMatrix ans = new DenseMatrix( m1.NumRows , m1.NumCols );
            for ( int i = 0 ; i < ans.NumRows ; i++ )
                for ( int j = 0 ; j < ans.NumCols ; j++ )
                    ans[i , j] = m1[i , j] / m2[i , j];

            return ans;
        }

        public static DenseMatrix Abs ( DenseMatrix m1 )
        {
            DenseMatrix ans = new DenseMatrix( m1.NumRows , m1.NumCols );
            for ( int i = 0 ; i < ans.NumRows ; i++ )
                for ( int j = 0 ; j < ans.NumCols ; j++ )
                    ans[i , j] = Math.Abs( m1[i , j] );

            return ans;
        }

        public static DenseMatrix Pow ( DenseMatrix m1 , int power )
        {
            if ( power < 0 )
            {
                if ( power == -1 ) return m1.Inverse;
                else throw new ArgumentException( "Matrix power must be a positive integer (or -1 for inverse)." );
            }

            if ( !m1.IsSquare )
                throw new InvalidOperationException( "Only square matrices may be raised to a power." );

            DenseMatrix ans = m1.Copy();
            if ( power > 1 ) ans *= DenseMatrix.Pow( m1 , power - 1 );

            return ans;
        }
    }


    class BandSymMatrix
    {
        private double[,] data;
        private int nrows;
        private int hbw;

        private static Random rand = new Random();

        public BandSymMatrix ( int nrows , int bandwidth , bool identity = false )
        {
            this.nrows = nrows;
            this.hbw = bandwidth / 2 + 1;

            data = new double[nrows , hbw];

            if ( identity ) FillIdentity();
        }

        public int NumRows { get { return this.nrows; } }
        public int NumCols { get { return this.nrows; } }
        public int HBW { get { return this.hbw; } }

        public double this[int i , int j , bool checkindex = false]
        {
            get
            {
                if ( checkindex ) if ( !this.checkIndex( i , j ) ) throw new IndexOutOfRangeException();

                // Check order of indices (symmetry)
                return (i > j)
                    ? (i - j) < hbw ? data[j , i - j] : 0.0
                    : (j - i) < hbw ? data[i , j - i] : 0.0;
            }

            set
            {
                if ( checkindex ) if ( !this.checkIndex( i , j ) ) throw new IndexOutOfRangeException();

                // if new entry is outside band, increase bandwidth
                if ( Math.Abs( i - j ) >= hbw )
                {
                    int prevHBW = hbw;

                    double[,] prevData = data.Clone() as double[,];

                    hbw += j - i;
                    data = new double[nrows , hbw];
                    for ( int m = 0 ; m < nrows ; m++ )
                        for ( int n = 0 ; n < prevHBW ; n++ )
                            data[m , n] = prevData[m , n];
                }

                // Check order of indices (symmetry)
                if ( i > j ) data[j , i - j] = value;
                else data[i , j - i] = value;
            }

        }

        public DenseMatrix this[int i , bool getcol = false]
        {
            get
            {
                if ( getcol )
                {
                    DenseMatrix ans = new DenseMatrix( nrows , 1 );
                    for ( int j = 0 ; j < nrows ; j++ ) ans[j , 0] = this[j , i];
                    return ans;
                }
                else
                {
                    DenseMatrix ans = new DenseMatrix( 1 , nrows );
                    for ( int j = 0 ; j < nrows ; j++ ) ans[0 , j] = this[i , j];
                    return ans;
                }
            }
        }

        private bool checkIndex ( int i , int j )
        {
            return !(i >= nrows || i < 0 || j >= nrows || j < 0);
        }

        public BandSymMatrix Transpose { get { return this.Copy(); } }

        public bool IsSquare { get { return true; } }
        public bool IsSymmetric { get { return true; } }

        public BandSymMatrix[] Cholesky
        {
            get
            {
                try
                {
                    // initialize L and D matrices for this = [L][D][L]^T decomposition
                    BandSymMatrix L = new BandSymMatrix( nrows , hbw * 2 - 1 , /* identity = */ true );
                    BandSymMatrix D = new BandSymMatrix( nrows , 1 );

                    double offdiagsum , diagsum;
                    for ( int i = 0 ; i < nrows ; i++ )   // loop through rows
                    {
                        // loop through off-diag columns
                        for ( int j = Math.Max( 0 , i - hbw + 1 ) ; j < i ; j++ )
                        {
                            offdiagsum = 0;
                            for ( int k = Math.Max( 0 , i - hbw + 1 ) ; k < j ; k++ )
                                offdiagsum += L[i , k] * L[j , k] * D[k , k];

                            L[i , j] = (this[i , j] - offdiagsum) / D[j , j];
                        }

                        // diagonal entries
                        diagsum = 0;
                        for ( int k = Math.Max( 0 , i - hbw + 1 ) ; k < i ; k++ )
                            diagsum += L[i , k] * L[i , k] * D[k , k];

                        D[i , i] = this[i , i] - diagsum;

                        if ( D[i , i] <= 0 )
                            throw new InvalidOperationException( "Error! Cannot obtain Cholesky decomposition because matrix is not positive definite." );
                    }

                    return new BandSymMatrix[] { L , D };
                }
                catch ( InvalidOperationException e )
                {
                    MessageBox.Show( e.Message , "Error" );
                    return null;
                }
            }
        }

        public double Determinant
        {
            get
            {
                // 1x1 matrix
                if ( nrows == 1 ) return this[0 , 0];

                // 2x2 matrix
                else if ( nrows == 2 ) return this[0 , 0] * this[1 , 1] - this[0 , 1] * this[0 , 1];

                // nxn matrix using Cholesky decomposition (product of diagonal elements of D)
                else
                {
                    BandSymMatrix D = Cholesky[1];
                    double det = 1.0;
                    for ( int i = 0 ; i < nrows ; i++ ) det *= D[i , i];
                    return det;
                }
            }
        }

        public DenseMatrix Inverse
        {
            get
            {
                // obtain Cholesky decomposition, this = [L][D][L]^T
                BandSymMatrix[] LD = Cholesky;

                // ensure matrix is non singular
                for ( int i = 0 ; i < nrows ; i++ )
                    if ( LD[1][i , i] < 1e-5 )
                        throw new InvalidOperationException( "Cannot compute inverse because matrix is singular." );

                // initialize inverse with appropriate solution vectors
                DenseMatrix inv = new DenseMatrix( nrows , nrows , /* identity = */ true );

                // loop through column solution vectors
                for ( int k = 0 ; k < nrows ; k++ )
                {
                    // forward substitution
                    inv[0 , k] /= LD[1][0 , 0];
                    for ( int i = 1 ; i < nrows ; i++ )
                    {
                        for ( int j = Math.Max( 0 , i - HBW + 1 ) ; j < i ; j++ )
                            inv[i , k] -= LD[0][i , j] * LD[1][j , j] * inv[j , k];

                        inv[i , k] /= LD[1][i , i];
                    }

                    // back substitution
                    for ( int i = nrows - 2 ; i >= 0 ; i-- )
                    {
                        for ( int j = i + 1 ; j < Math.Min( i + hbw , nrows ) ; j++ )
                            inv[i , k] -= LD[0][j , i] * inv[j , k];       // reverse index for L since U=L^T
                    }
                }

                return inv;
            }
        }

        public void Print ( TextWriter tw )
        {
            for ( int i = 0 ; i < nrows ; i++ )
            {
                tw.Write( "[\t" );
                for ( int j = 0 ; j < nrows ; j++ ) tw.Write( String.Format( "{0:e6}\t" , this[i , j] ) );
                tw.WriteLine( "]" );
            }
        }

        public void PrintToFile ( string path , bool append = false )
        {
            using ( TextWriter tw = append ? File.AppendText( path ) : new StreamWriter( path ) )
            {
                for ( int i = 0 ; i < nrows ; i++ )
                {
                    for ( int j = 0 ; j < hbw ; j++ ) tw.Write( String.Format( "{0:e6}\t" , data[i , j] ) );
                    tw.WriteLine();
                }

                tw.WriteLine();
            }
        }

        public void PrintFullToFile ( string path , bool append = false )
        {
            using ( TextWriter tw = append ? File.AppendText( path ) : new StreamWriter( path ) )
            {
                for ( int i = 0 ; i < nrows ; i++ )
                {
                    for ( int j = 0 ; j < nrows ; j++ ) tw.Write( String.Format( "{0:e6}\t" , this[i , j] ) );
                    tw.WriteLine();
                }

                tw.WriteLine();
            }
        }

        public override string ToString ()
        {
            string output = "";
            for ( int i = 0 ; i < nrows ; i++ )
            {
                output += "[\t";
                for ( int j = 0 ; j < nrows ; j++ ) output += String.Format( "{0:e6}\t" , this[i , j] );
                output += "]\n";
            }
            return output;
        }

        public BandSymMatrix Copy ()
        {
            BandSymMatrix copy = new BandSymMatrix( nrows , hbw * 2 - 1 );
            for ( int i = 0 ; i < nrows ; i++ )
                for ( int j = i ; j < Math.Min( i + hbw , nrows ) ; j++ )
                    copy[i , j] = this[i , j];

            return copy;
        }

        public void FillIdentity ( bool reinit = false )
        {
            if ( reinit )
            {
                // re-initialize data with bandwidth of 1
                hbw = 1;
                data = new double[nrows , 1];
            }

            for ( int i = 0 ; i < nrows ; i++ )
                for ( int j = 0 ; j < hbw ; j++ )
                    data[i , j] = j == 0 ? 1.0 : 0.0;
        }

        public void FillRandom ( double start = 0 , double end = 10 )
        {
            for ( int i = 0 ; i < nrows ; i++ )
                for ( int j = 0 ; j < hbw ; j++ )
                    data[i , j] = start + (end - start) * rand.NextDouble();
        }

        public void FillRandPosDef ( double start = 0 , double end = 10 )
        {
            // ensure all random values are non-negative
            if ( start < 0 || end < 0 )
                throw new ArgumentException( "Both start and end must be positive values." );

            // re-initialize data with bandwidth of 3
            hbw = 2;
            data = new double[nrows , hbw];

            // generate a random set of axial springs in series fixed at one end, free at other
            double[] rands = new double[nrows];
            for ( int i = 0 ; i < nrows ; i++ ) rands[i] = start + (end - start) * rand.NextDouble();

            // build "stiffness matrix" for this system
            for ( int i = 0 ; i < nrows ; i++ )
            {
                for ( int j = i ; j < Math.Min( i + hbw , nrows ) ; j++ )
                {
                    if ( i == j )
                    {
                        this[i , j] = rands[i];
                        if ( i < nrows - 1 ) this[i , j] += rands[i + 1];
                    }
                    else this[i , j] = -rands[i + 1];
                }
            }
        }



        // ------------------------------------------------
        // OPERATORS
        // ------------------------------------------------

        public static bool operator == ( BandSymMatrix m1 , BandSymMatrix m2 )
        {
            if ( m1.NumRows != m2.NumRows ) return false;

            double toler = 1e-8;
            for ( int i = 0 ; i < m1.NumRows ; i++ )
                for ( int j = i ; j < Math.Min( i + Math.Max( m1.HBW , m2.HBW ) , m1.NumRows ) ; j++ )
                    if ( Math.Abs( m1[i , j] - m2[i , j] ) > toler ) return false;

            return true;
        }

        public static bool operator != ( BandSymMatrix m1 , BandSymMatrix m2 )
        {
            return !(m1 == m2);
        }

        public override bool Equals ( object obj )
        {
            return base.Equals( obj );
        }

        public override int GetHashCode ()
        {
            return base.GetHashCode();
        }

        public static BandSymMatrix operator + ( BandSymMatrix m1 )
        {
            return m1.Copy();
        }

        public static BandSymMatrix operator - ( BandSymMatrix m1 )
        {
            BandSymMatrix ans = new BandSymMatrix( m1.NumRows , m1.HBW * 2 - 1 );
            for ( int i = 0 ; i < ans.NumRows ; i++ )
                for ( int j = i ; j < Math.Min( i + ans.HBW , ans.NumRows ) ; j++ )
                    ans[i , j] = -m1[i , j];

            return ans;
        }

        public static BandSymMatrix operator + ( BandSymMatrix m1 , BandSymMatrix m2 )
        {
            if ( m1.NumRows != m2.NumRows )
                throw new ArgumentException( "Matrices must have same dimensions to add them." );

            BandSymMatrix ans = new BandSymMatrix( m1.NumRows , Math.Max( m1.HBW , m2.HBW ) * 2 - 1 );
            for ( int i = 0 ; i < ans.NumRows ; i++ )
                for ( int j = i ; j < Math.Min( i + ans.HBW , ans.NumRows ) ; j++ )
                    ans[i , j] = m1[i , j] + m2[i , j];

            return ans;
        }

        public static BandSymMatrix operator + ( double value , BandSymMatrix m1 )
        {
            BandSymMatrix ans = new BandSymMatrix( m1.NumRows , m1.HBW * 2 - 1 );
            for ( int i = 0 ; i < ans.NumRows ; i++ )
                for ( int j = i ; j < Math.Min( i + ans.HBW , ans.NumRows ) ; j++ )
                    ans[i , j] = m1[i , j] + value;

            return ans;
        }

        public static BandSymMatrix operator + ( BandSymMatrix m1 , double value )
        {
            return value + m1;
        }

        public static BandSymMatrix operator - ( BandSymMatrix m1 , BandSymMatrix m2 )
        {
            if ( m1.NumRows != m2.NumRows )
                throw new ArgumentException( "Matrices must have same dimensions to add them." );

            BandSymMatrix ans = new BandSymMatrix( m1.NumRows , Math.Max( m1.HBW , m2.HBW ) * 2 - 1 );
            for ( int i = 0 ; i < ans.NumRows ; i++ )
                for ( int j = i ; j < Math.Min( i + ans.HBW , ans.NumRows ) ; j++ )
                    ans[i , j] = m1[i , j] - m2[i , j];

            return ans;
        }

        public static BandSymMatrix operator - ( double value , BandSymMatrix m1 )
        {
            BandSymMatrix ans = new BandSymMatrix( m1.NumRows , m1.HBW * 2 - 1 );
            for ( int i = 0 ; i < ans.NumRows ; i++ )
                for ( int j = i ; j < Math.Min( i + ans.HBW , ans.NumRows ) ; j++ )
                    ans[i , j] = value - m1[i , j];

            return ans;
        }

        public static BandSymMatrix operator - ( BandSymMatrix m1 , double value )
        {
            return m1 + (-value);
        }

        public static DenseMatrix operator * ( BandSymMatrix m1 , BandSymMatrix m2 )
        {
            if ( m1.NumRows != m2.NumRows )
                throw new InvalidOperationException( "Inner matrix dimensions must match to perform matrix multiplication." );

            int newHBW = (2 * (m1.HBW + m2.HBW) - 3) / 2 + 1;
            int maxPrevHBW = Math.Max( m1.HBW , m2.HBW );

            DenseMatrix ans = new DenseMatrix( m1.NumRows , m2.NumCols );
            for ( int i = 0 ; i < ans.NumRows ; i++ )
                for ( int j = Math.Max( 0 , i - newHBW + 1 ) ; j < Math.Min( i + newHBW , ans.NumCols ) ; j++ )
                    for ( int k = Math.Max( 0 , i - maxPrevHBW + 1 ) ; k < Math.Min( i + maxPrevHBW , ans.NumRows ) ; k++ )
                        ans[i , j] += m1[i , k] * m2[k , j];

            return ans;
        }

        public static BandSymMatrix operator * ( BandSymMatrix m1 , double value )
        {
            BandSymMatrix ans = new BandSymMatrix( m1.NumRows , m1.HBW * 2 - 1 );
            for ( int i = 0 ; i < ans.NumRows ; i++ )
                for ( int j = i ; j < Math.Min( i + ans.HBW , ans.NumRows ) ; j++ )
                    ans[i , j] = value * m1[i , j];

            return ans;
        }

        public static BandSymMatrix operator * ( double value , BandSymMatrix m1 )
        {
            return m1 * value;
        }

        public static BandSymMatrix ElementMult ( BandSymMatrix m1 , BandSymMatrix m2 )
        {
            if ( m1.NumRows != m2.NumRows )
                throw new InvalidOperationException( "Matrix dimensions must match to perform element-wise multiplication." );

            BandSymMatrix ans = new BandSymMatrix( m1.NumRows , Math.Min( m1.HBW , m2.HBW ) * 2 - 1 );
            for ( int i = 0 ; i < ans.NumRows ; i++ )
                for ( int j = i ; j < Math.Min( i + ans.HBW , ans.NumRows ) ; j++ )
                    ans[i , j] = m1[i , j] * m2[i , j];

            return ans;
        }

        public static DenseMatrix operator / ( BandSymMatrix m1 , BandSymMatrix m2 )
        {
            return m2.Inverse * (DenseMatrix) m1;
        }

        public static void SolveInPlaceCholesky ( BandSymMatrix[] chol , DenseMatrix b , ref DenseMatrix ans )
        {
            int hbw = chol[0].HBW , nrows = chol[0].NumRows;

            // forward substitution
            ans[0 , 0] = b[0 , 0] / chol[1][0 , 0];
            for ( int i = 1 ; i < nrows ; i++ )
            {
                ans[i , 0] = b[i , 0];
                for ( int j = Math.Max( 0 , i - hbw + 1 ) ; j < i ; j++ )
                    ans[i , 0] -= ans[j , 0] * chol[0][i , j] * chol[1][j , j];
                ans[i , 0] /= chol[1][i , i];
            }

            // back substitution
            for ( int i = nrows - 2 ; i >= 0 ; i-- )
                for ( int j = i + 1 ; j < Math.Min( i + hbw , nrows ) ; j++ )
                    ans[i , 0] -= ans[j , 0] * chol[0][j , i];
        }

        public static void SolveInPlaceGaussSeidel ( BandSymMatrix A , DenseMatrix b , ref DenseMatrix ans )
        {
            // initialize matrices for solution
            DenseMatrix newans = new DenseMatrix( ans.NumRows , ans.NumCols );
            for ( int i = 0 ; i < ans.NumRows ; i++ ) ans[i , 0] = b[i , 0] / A[i , i];

            // iteratively solve using Gauss-Seidel method
            double eps_s = 1e-10 , eps_a = 1;
            double delta , deltanorm , norm;
            while ( eps_a > eps_s )
            {
                // loop through rows, updating guesses
                for ( int i = 0 ; i < ans.NumRows ; i++ )
                {
                    newans[i , 0] = b[i , 0];

                    for ( int j = Math.Max( 0 , i - A.HBW + 1 ) ; j < i ; j++ )
                        newans[i , 0] -= A[i , j] * newans[j , 0];

                    for ( int j = i + 1 ; j < Math.Min( i + A.HBW , A.NumRows ) ; j++ )
                        newans[i , 0] -= A[i , j] * ans[j , 0];

                    newans[i , 0] /= A[i , i];
                }

                // compute approximation error and update guesses
                deltanorm = 0; norm = 0;
                for ( int i = 0 ; i < ans.NumRows ; i++ )
                {
                    delta = newans[i , 0] - ans[i , 0];   // compute change in answer

                    deltanorm += delta * delta;         // increment error sums
                    norm += ans[i , 0] * ans[i , 0];

                    ans[i , 0] += delta;                 // update guess
                }
                eps_a = Math.Sqrt( deltanorm / norm );    // compute approximation error
            }
        }

        public static BandSymMatrix operator / ( BandSymMatrix m1 , double value )
        {
            return m1 * (1 / value);
        }

        public static BandSymMatrix ElementDiv ( BandSymMatrix m1 , BandSymMatrix m2 )
        {
            if ( m1.NumRows != m2.NumRows )
                throw new InvalidOperationException( "Matrix dimensions must match to perform element-wise division." );

            if ( m1.HBW > m2.HBW )
                throw new DivideByZeroException( "Bandwidth of second argument must be >= bandwidth of first argument." );

            BandSymMatrix ans = new BandSymMatrix( m1.NumRows , m1.HBW * 2 - 1 );
            for ( int i = 0 ; i < ans.NumRows ; i++ )
                for ( int j = i ; j < Math.Min( i + ans.HBW , ans.NumRows ) ; j++ )
                    ans[i , j] = m1[i , j] / m2[i , j];

            return ans;
        }

        public static BandSymMatrix Abs ( BandSymMatrix m1 )
        {
            BandSymMatrix ans = new BandSymMatrix( m1.NumRows , m1.HBW * 2 - 1 );
            for ( int i = 0 ; i < ans.NumRows ; i++ )
                for ( int j = i ; j < Math.Min( i + ans.HBW , ans.NumRows ) ; j++ )
                    ans[i , j] = Math.Abs( m1[i , j] );

            return ans;
        }

        public static BandSymMatrix Pow ( BandSymMatrix m1 , int power )
        {
            if ( power < 0 )
                throw new InvalidOperationException( "Can only raise matrix to positive integer power." );

            BandSymMatrix ans = m1.Copy();

            int newHBW;
            BandSymMatrix newAns;
            while ( --power > 0 )
            {
                newHBW = (2 * (ans.HBW + m1.HBW) - 3) / 2 + 1;
                newAns = new BandSymMatrix( m1.NumRows , newHBW * 2 - 1 );

                // matrix multiplication
                for ( int i = 0 ; i < ans.NumRows ; i++ )
                    for ( int j = i ; j < Math.Min( i + newAns.HBW , ans.NumRows ) ; j++ )
                        for ( int k = Math.Max( 0 , i - ans.HBW + 1 ) ; k < Math.Min( i + ans.HBW , ans.NumRows ) ; k++ )
                            newAns[i , j] += ans[i , k] * m1[k , j];

                ans = newAns.Copy();
            }

            return ans;
        }
    }
}