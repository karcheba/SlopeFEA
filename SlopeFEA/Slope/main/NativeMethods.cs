using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace SlopeFEA
{
    public partial class NativeMethods
    {
        [System.Runtime.InteropServices.DllImportAttribute("user32.dll", EntryPoint = "SetCursorPos")]
        [return: System.Runtime.InteropServices.MarshalAsAttribute(System.Runtime.InteropServices.UnmanagedType.Bool)]
        public static extern bool SetCursorPos(int X, int Y);
    }
}
