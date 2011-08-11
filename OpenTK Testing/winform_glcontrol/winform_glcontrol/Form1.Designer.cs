namespace winform_glcontrol
{
    partial class Form1
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose ( bool disposing )
        {
            if ( disposing && (components != null) )
            {
                components.Dispose();
            }
            base.Dispose( disposing );
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent ()
        {
            this.glControl1 = new OpenTK.GLControl();
            this.label1 = new System.Windows.Forms.Label();
            this.SuspendLayout();
            // 
            // glControl1
            // 
            this.glControl1.BackColor = System.Drawing.Color.Black;
            this.glControl1.Location = new System.Drawing.Point( 30 , 30 );
            this.glControl1.Margin = new System.Windows.Forms.Padding( 30 );
            this.glControl1.Name = "glControl1";
            this.glControl1.Size = new System.Drawing.Size( 215 , 200 );
            this.glControl1.TabIndex = 0;
            this.glControl1.VSync = false;
            this.glControl1.Load += new System.EventHandler( this.glControl1_Load );
            this.glControl1.Paint += new System.Windows.Forms.PaintEventHandler( this.glControl1_Paint );
            this.glControl1.KeyDown += new System.Windows.Forms.KeyEventHandler( this.glControl1_KeyDown );
            this.glControl1.Resize += new System.EventHandler( this.glControl1_Resize );
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point( 110 , 229 );
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size( 46 , 17 );
            this.label1.TabIndex = 1;
            this.label1.Text = "label1";
            // 
            // Form1
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF( 8F , 16F );
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size( 282 , 255 );
            this.Controls.Add( this.label1 );
            this.Controls.Add( this.glControl1 );
            this.Name = "Form1";
            this.Text = "Form1";
            this.Resize += new System.EventHandler( this.Form1_Resize );
            this.ResumeLayout( false );
            this.PerformLayout();

        }

        #endregion

        private OpenTK.GLControl glControl1;
        private System.Windows.Forms.Label label1;
    }
}

