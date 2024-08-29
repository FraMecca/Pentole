module internal Pentole.Native

open System
open System.Runtime.InteropServices


[<DllImport("libc", EntryPoint = "realpath", SetLastError = true, CharSet = CharSet.Auto, CallingConvention = CallingConvention.Cdecl)>]
extern nativeint _realpath (string _path, nativeint _resolved_path)

[<DllImport("libc", EntryPoint = "strerror", SetLastError = true, CharSet = CharSet.Auto, CallingConvention = CallingConvention.Cdecl)>]
extern nativeint _strerror (int _errnum)


let realpath (path: string) =
    let ptr = _realpath(path, IntPtr.Zero)
    if ptr = IntPtr.Zero then
        let errno = Marshal.GetLastWin32Error()
        let error = Marshal.PtrToStringAuto(_strerror errno)
        Error $"realpath failed: {error} ({errno})"
    else
        let result = Marshal.PtrToStringAuto ptr
        Marshal.FreeHGlobal ptr
        Ok result
