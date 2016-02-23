program NSChrmBrowser;

{$R 'administrator.res' '..\ShellPro\administrator.rc'}

uses
  ExceptionLog,
  Forms,
  Windows,
  SysUtils,
  Registry,
  ceffilescheme in 'filescheme\ceffilescheme.pas',
  Generics.Collections,
  NSDATSettingsUtils in '..\Framework\Settings\NSDATSettingsUtils.pas',
  MyTypes in '..\Common\MyTypes.pas',
  LogFile in '..\ShellPro\LogFile.pas',
  Utilities in '..\Common\Utilities.pas',
  Constants in '..\Common\Constants.pas',
  ceflib,
  uFormDCEWebBrowser in 'uFormDCEWebBrowser.pas',
  FrameWB in 'FrameWB.pas' {WBFrame: TFrame};

{.$R *.RES}

var
  RemoteDATFile : string;

  //mh 10/19/2013
  MyMutex : THandle;
  MyMutexName : string;


procedure RegisterSchemes(const registrar: ICefSchemeRegistrar);
begin
  registrar.AddCustomScheme('local', True, True, False);
end;

begin

  //mh 10/19/2013
  //////////////////////////////////////////////////////

  MyMutexName := 'NSChrmBrowser.exe';
  MyMutex := CreateMutex(nil, False, PChar(MyMutexName));
  if GetLastError = Error_Already_Exists then begin
    // Already Running - Outtahere...
    Exit;
  end;
  //////////////////////////////////////////////////////


  {$IFDEF DATSETTINGS}
  Dic := TDictionary<String,Tv6Setting>.Create;

  //determine DAT settings file to use
  //if a Remote file is defined compare locqal/remote file last modified
  //dates.  If remote file is newer, copy to local NetStopPro.dat
  //ALWAYS actually use the local NetStopPro.dat

  DATSETTINGSPATH := ExtractFilePath(Application.ExeName);

  with TRegistry.Create do
  try
    RootKey := MyRootKey;
    if OpenKey(NETSTOP_BASE_KEY, False) then
    begin
      //Only do this if not MASTER Kiosk and UseRemote is True
      if (ValueExists('KIOSKIsMaster') and
          ValueExists('KIOSKUseRemote') and
          (not ReadBool('KIOSKIsMaster')) and
          ReadBool('KIOSKUseRemote')) then
      begin
        if (ValueExists('DATFilePath') and ValueExists('DATFileName')) then
        begin
          if ('' <> ReadString('DATFilePath')) and ('' <> ReadString('DATFileName')) then
          begin
            RemoteDATFile := ReadString('DATFilePath')+ ReadString('DATFileName');
            if FileExists(RemoteDATFile) then
            begin
              if GetFileModDate(RemoteDATFile) > GetFileModDate(DATSETTINGSPATH + DATSETTINGSFILENAME) then
                CopyFile(pChar(RemoteDATFile),PChar(DATSETTINGSPATH + DATSETTINGSFILENAME),False);
            end;
          end;
        end;
      end;
      CloseKey;
    end;
  finally
    free;
  end;

  NS_OpenSettings;
  {$ENDIF}

  //CefCache := 'cache';
  CefOnRegisterCustomSchemes := RegisterSchemes;
  //CefSingleProcess := False;
  //if not CefLoadLibDefault then
   // Exit;

  //CefRegisterSchemeHandlerFactory('local', '', TFileScheme);

  ShellProLog.Initialise(0, utNA);

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Netstop Chromium Browser';
  Application.CreateForm(TFormDCEWebBrowser, FormDCEWebBrowser);
  Application.Run;
end.
