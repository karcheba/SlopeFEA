using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using OpenTK;

namespace displaydev
{
    class Program
    {
        static void Main ( string[] args )
        {
            foreach ( DisplayDevice device in DisplayDevice.AvailableDisplays )
            {
                Console.WriteLine( device.IsPrimary );
                Console.WriteLine( device.Bounds );
                Console.WriteLine( device.RefreshRate );
                Console.WriteLine( device.BitsPerPixel );
                foreach ( DisplayResolution res in device.AvailableResolutions )
                {
                    Console.WriteLine( res );
                }
            }

            IList<DisplayDevice> devices = DisplayDevice.AvailableDisplays;

            devices[0].ChangeResolution( 800 , 600 , 32 , 60 );

            Console.ReadKey();

            foreach ( DisplayDevice device in DisplayDevice.AvailableDisplays )
            {
                device.RestoreResolution();
            }

            Console.ReadKey();
        }
    }
}
