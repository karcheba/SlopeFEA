using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using OpenTK;
using OpenTK.Graphics;
using OpenTK.Graphics.OpenGL;
using System.Diagnostics;

namespace winform_glcontrol
{
    public partial class Form1 : Form
    {
        bool loaded = false;
        int x = 0;
        float theta = 0;
        double accumulator = 0;
        int idleCounter = 0;
        Stopwatch sw = new Stopwatch();

        public Form1 ()
        {
            InitializeComponent();
        }

        private void glControl1_Load ( object sender , EventArgs e )
        {
            loaded = true;
            GL.ClearColor( Color.Black );
            SetupViewport();
            Application.Idle += new EventHandler( Application_Idle );
            sw.Start();
        }

        private void SetupViewport ()
        {
            int w = glControl1.Width;
            int h = glControl1.Height;
            GL.MatrixMode( MatrixMode.Projection );
            GL.LoadIdentity();
            GL.Ortho( 0 , w , 0 , h , -1 , 1 );
            GL.Viewport( 0 , 0 , w , h );
        }

        private void glControl1_Paint ( object sender , PaintEventArgs e )
        {
            Render();
        }

        private void glControl1_KeyDown ( object sender , KeyEventArgs e )
        {
            if ( e.KeyCode == Keys.Space )
            {
                x++;
                glControl1.Invalidate();
            }
            else if ( e.KeyCode == Keys.Add )
            {
                theta++;
                glControl1.Invalidate();
            }
        }

        private void Application_Idle ( object sender , EventArgs e )
        {
            double milliseconds = ComputeTimeSlice();
            Accumulate( milliseconds );
            Animate( milliseconds );
        }

        private double ComputeTimeSlice ()
        {
            sw.Stop();
            double timeslice = sw.Elapsed.TotalMilliseconds;
            sw.Reset();
            sw.Start();
            return timeslice;
        }

        private void Animate ( double milliseconds )
        {
            float deltaTheta = (float) milliseconds / 20.0f;
            theta += deltaTheta;
            glControl1.Invalidate();
        }

        private void Accumulate ( double milliseconds )
        {
            idleCounter++;
            accumulator += milliseconds;
            if ( accumulator > 1000 )
            {
                label1.Text = idleCounter.ToString();
                accumulator -= 1000;
                idleCounter = 0;
            }
        }

        private void Render ()
        {
            if ( !loaded ) return;

            GL.Clear( ClearBufferMask.ColorBufferBit | ClearBufferMask.DepthBufferBit );

            GL.MatrixMode( MatrixMode.Modelview );
            GL.LoadIdentity();

            GL.Translate( x , 0 , 0 );
            GL.Rotate( theta , Vector3.UnitZ );

            GL.Begin( BeginMode.Triangles );

            GL.Color3( Color.Red ); GL.Vertex2( glControl1.Width / 3 , glControl1.Height / 3 );
            GL.Color3( Color.Yellow ); GL.Vertex2( glControl1.Width * 2 / 3 , glControl1.Height / 3 );
            GL.Color3( Color.Blue ); GL.Vertex2( glControl1.Width * 2 / 3 , glControl1.Height * 2 / 3 );

            GL.End();

            glControl1.SwapBuffers();
        }

        private void glControl1_Resize ( object sender , EventArgs e )
        {
            SetupViewport();
            glControl1.Invalidate();
        }

        private void Form1_Resize ( object sender , EventArgs e )
        {
            if ( loaded )
            {
                glControl1.Width = this.ClientSize.Width - 60;
                glControl1.Height = this.ClientSize.Height - 60;
            }
        }
    }
}
