unit glSysInf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, extctrls;

type

  TglSysInfo = class(TComponent)
  private
    {data fields for properties}
    FMemoryLoad      : integer;
    FTotalPhys       : integer;
    FAvailPhys       : integer;
    FTotalPageFile   : integer;
    FAvailPageFile   : integer;
    FTotalVirtual    : integer;
    FAvailVirtual    : integer;
    FColorDepth      : integer;
    FSystemFont      : string;
    FOSPlatform      : string;
    FVRefreshRate    : integer;
    FGraphicResolution: string;
    FCPUKind         : integer;
    FCPUName         : string;
    FComputerName    : string;
    FUserName        : string;
    iNone            : integer;
    sNone            : string;    
  protected
    procedure Loaded; override;
  public
    procedure Refresh;
    constructor Create(AOwner: TComponent); override;
  published
    property MemoryLoad: integer read FMemoryLoad write iNone stored false;
    property TotalPhys: integer read FTotalPhys write iNone stored false;
    property AvailPhys: integer read FAvailPhys write iNone stored false;
    property TotalPageFile: integer read FTotalPageFile write iNone stored false;
    property AvailPageFile: integer read FAvailPageFile write iNone stored false;
    property TotalVirtual: integer read FTotalVirtual write iNone stored false;
    property AvailVirtual: integer read FAvailVirtual write iNone stored false;

    property CPUKind: Integer read FCPUKind write iNone stored false;
    property CPUName: string read FCPUName write sNone stored false;
    property ColorDepth: integer read FColorDepth write iNone stored false;
    property SystemFont: string read FSystemFont write sNone stored false;
    property OSPlatform: string read FOSPlatform write sNone stored false;
    property VRefreshRate: integer read FVRefreshRate write iNone stored false;
    property GraphicResolution: string read FGraphicResolution write sNone stored false;
    property ComputerName: string read FComputerName write sNone stored false;
    property UserName: string read FUserName write sNone stored false;
  end;

const
  i8086 = 1; { & 8088 CPU }
  i80286 = 2;
  i80386 = 3;
  i80486 = 4;
  iPentium = 5; { P5 - Pentium }
  iPentiumPro = 6; { P6 - Pentium Pro & Celeron}

procedure Register;

implementation
uses glUtils;

constructor TglSysInfo.Create(AOwner: TComponent);
begin
  inherited;
  if csDesigning in ComponentState then Refresh;
end;

procedure TglSysInfo.Loaded;
begin
  Refresh;
end;

procedure TglSysInfo.Refresh;
var
  DC: HDC;
  OSVersionInfo: TOSVersionInfo;
  SI: TSystemInfo;
  MS: TMemoryStatus;
begin
  MS.dwLength := SizeOf(MS);
  GlobalMemoryStatus(MS);
  OSVersionInfo.dwOSVersionInfoSize := sizeof(OSVersionInfo);
  GetVersionEx(OSVersionInfo);
  GetSystemInfo(SI);
  DC := GetDC(0);

  FMemoryLoad := MS.dwMemoryLoad;
  FTotalPhys := MS.dwTotalPhys;
  FAvailPhys := MS.dwAvailPhys;
  FTotalPageFile := MS.dwTotalPageFile;
  FAvailPageFile := MS.dwAvailPageFile;
  FTotalVirtual := MS.dwTotalVirtual;
  FAvailVirtual := MS.dwAvailVirtual;


  FCPUKind := SI.wProcessorLevel;

  case SI.wProcessorLevel of
//    i8086: Result := '8086';
//    i80286: Result := '80286';
    i80386: FCPUName := '80386';
    i80486: FCPUName := '80486';
    iPentium: FCPUName := 'Pentium';
    iPentiumPro: FCPUName := 'Pentium Pro/Celeron';
  else
    FCPUName := Format('P%d', [SI.wProcessorLevel]);
  end;

  if OSVersionInfo.dwPlatformId = VER_PLATFORM_WIN32_NT then FOSPlatform := 'NT'
  else if OSVersionInfo.dwPlatformId = VER_PLATFORM_WIN32_WINDOWS then FOSPlatform := '95';

  FVRefreshRate := GetDeviceCaps(DC, VREFRESH);
  FColorDepth := GetDeviceCaps(DC, BITSPIXEL);

  FGraphicResolution := Format('%dx%d', [GetDeviceCaps(DC, HORZRES), GetDeviceCaps(DC, VERTRES)]);

  if GetDeviceCaps(DC, LOGPIXELSX) = 96 then FSystemFont := 'SmallFont'
  else if GetDeviceCaps(DC, LOGPIXELSX) = 120 then FSystemFont := 'BigFont';

  FComputerName := glUtils.ComputerName;
  FUserName := glUtils.UserName;

  ReleaseDC(0, DC);
end;

{registration procedure}

procedure Register;
begin
  RegisterComponents ('Globus Components', [TglSysInfo]);
end;

end.

