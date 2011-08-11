using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Drawing;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using OpenTK;
using OpenTK.Graphics;
using OpenTK.Graphics.OpenGL;
using System.Diagnostics;

namespace wpf_glcontrol
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        GLControl glcontrol;
        bool loaded = false;

        public MainWindow ()
        {
            InitializeComponent();

            glcontrol = new GLControl();
            glcontrol.Location = new System.Drawing.Point( 0 , 0 );
            glcontrol.Margin = new System.Windows.Forms.Padding( 0 , 0 , 0 , 0 );
            glcontrol.Load += new EventHandler( glcontrol_Load );
            glcontrol.Resize += new EventHandler( glcontrol_Resize );
            glcontrol.Paint += new System.Windows.Forms.PaintEventHandler( glcontrol_Paint );
            windowsFormsHost1.Child = glcontrol;
        }

        void glcontrol_Paint ( object sender , System.Windows.Forms.PaintEventArgs e )
        {
            Render();
        }

        private void Render ()
        {
            if ( !loaded ) return;

            GL.Clear( ClearBufferMask.ColorBufferBit | ClearBufferMask.DepthBufferBit );

            GL.MatrixMode( MatrixMode.Modelview );
            GL.LoadIdentity();

            GL.Begin( BeginMode.Triangles );

            GL.Color3( Color.Red ); GL.Vertex2( glcontrol.Width / 3 , glcontrol.Height / 3 );
            GL.Color3( Color.Yellow ); GL.Vertex2( glcontrol.Width * 2 / 3 , glcontrol.Height / 3 );
            GL.Color3( Color.Blue ); GL.Vertex2( glcontrol.Width * 2 / 3 , glcontrol.Height * 2 / 3 );

            GL.End();

            glcontrol.SwapBuffers();
        }

        void glcontrol_Load ( object sender , EventArgs e )
        {
            loaded = true;
            GL.ClearColor( Color.Black );
            SetupViewport();
        }

        private void SetupViewport ()
        {
            int w = glcontrol.Width;
            int h = glcontrol.Height;
            GL.MatrixMode( MatrixMode.Projection );
            GL.LoadIdentity();
            GL.Ortho( 0 , w , 0 , h , -1 , 1 );
            GL.Viewport( 0 , 0 , w , h );
        }

        void glcontrol_Resize ( object sender , EventArgs e )
        {
            SetupViewport();
            glcontrol.Invalidate();
            glcontrol.Update();
            windowsFormsHost1.InvalidateVisual();
            windowsFormsHost1.InvalidateArrange();
        }

        private void windowsFormsHost1_SizeChanged ( object sender , SizeChangedEventArgs e )
        {
            if ( loaded )
            {
                glcontrol.Width = (int) windowsFormsHost1.Width;
                glcontrol.Height = (int) windowsFormsHost1.Height;
            }
        }
    }
}
