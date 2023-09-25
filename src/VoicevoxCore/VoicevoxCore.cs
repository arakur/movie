namespace VoicevoxCore
{

  using System;
  using System.IO;
  using System.Runtime.InteropServices;

  public class VoicevoxCore : IDisposable
  {
    static string basePath = AppDomain.CurrentDomain.BaseDirectory;
    static string dllPath = Path.Combine(basePath, "../../../../../VoicevoxCoreWrapper/bin");
    const string Dll = "VoicevoxCoreWrapper.dll";

    [DllImport(Dll, CallingConvention = CallingConvention.Cdecl)]
    public static extern int InitializeVoicevox();

    [DllImport(Dll, CallingConvention = CallingConvention.Cdecl)]
    public static extern void FinalizeVoicevox();

    [DllImport(Dll, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Unicode)]
    public static extern int TextToSpeech(string text, int speaker_id, ref int length, ref IntPtr wav);

    [DllImport(Dll, CallingConvention = CallingConvention.Cdecl)]
    public static extern void FreeWav(IntPtr wav);

    [DllImport(Dll, CallingConvention = CallingConvention.Cdecl)]
    public static extern string GetMetasJson();

    // 

    [DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
    static extern bool SetDllDirectory(string lpPathName);

    //

    public VoicevoxCore()
    {
      Console.WriteLine("Initializing voicevox.");
      SetDllDirectory(dllPath);
      InitializeVoicevox();
    }

    public void Dispose()
    {
      Console.WriteLine("Finalizing voicevox.");
      FinalizeVoicevox();
    }

    // 

    public (byte[], bool) Synthesize(string text, int speaker_id)
    {
      int length = 0;
      IntPtr wav = IntPtr.Zero;

      TextToSpeech(text, speaker_id, ref length, ref wav);

      if (wav != IntPtr.Zero)
      {
        byte[] buffer = new byte[length];
        Marshal.Copy(wav, buffer, 0, length);
        FreeWav(wav);

        return (buffer, true);
      }

      return (Array.Empty<byte>(), false);
    }
  }
}