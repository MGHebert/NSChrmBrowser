//
//              (C) Moonrise Systems Inc 1999
//               ALL RIGHTS RESERVED.
//__________________________________________________________________________
// Unit File  : DCEWebBrowser.PAS
// Author     : Yves Mailhot
// Modified   : MH - swapped NetstopBrowser for Delphi Chromium Embedded
//
// 06/28/2015 : MH - Swapped DCEF1 for DCEF3
//__________________________________________________________________________
//
// Description
//
//   This unit is the implementation of a Web Browser. It uses Delphi
//   Chromium Embedded component to display the web pages
//__________________________________________________________________________

unit uFormDCEWebBrowser;

{.$DEFINE TESTMODE}

interface

uses
  Windows, Messages, SysUtils, Classes, ALProgressBar, URLMon,
  IBEAntialiasButton, StatusBarPlus, CommCtrl, IBEButton, pngimage,
  Superbvlplus, TntComCtrls, TntStdCtrls, Dialogs, shellAPI, SHDocVw,
  ExtCtrls, ComCtrls, StdCtrls, OleCtrls, Controls, Forms, Graphics,
  NetStopBrowser, ActiveX, MSHTML, printers, LogFile, sndkey32, SHFolder,
  StrUtils, ksTimers, NSDatSettingsUtils, ImgList,
  Cromis.IPC, Cromis.Threading, MyTypes, TypInfo, ceflib, rkSmartTabs;

type


 //TOleAppPrintThread = class(TThread)
 // private
 //   { Private Declarations }
 // protected
 //   { Protected Declarations }
 // end;

  // Used to disable mouse wheel in combo box. Otherwise it navigates from page to page.
  TComboBox = class(StdCtrls.TComboBox)
  protected
    procedure ComboWndProc(var Message: TMessage; ComboWnd: hWnd; ComboProc: Pointer); override;
  end;

  TFormDCEWebBrowser = class(TForm)
    // Visual components.
    PanelButtons: TPanel;
    ComboBoxLocation: TComboBox;
    ButtonClose: TIBEAntialiasButton;
    ButtonBack: TIBEAntialiasButton;
    ButtonForward: TIBEAntialiasButton;
    ButtonStop: TIBEAntialiasButton;
    ButtonZoom: TIBEAntialiasButton;
    ButtonSearch: TIBEAntialiasButton;
    ButtonPrint: TIBEAntialiasButton;
    ButtonOpen: TIBEAntialiasButton;
    ButtonGoHome: TIBEAntialiasButton;
    ButtonNewWindow: TIBEAntialiasButton;
    StatusBarWebBrowser: TStatusBarPlus;
    PanelAnimation: TPanel;
    ImageAnimation: TImage;
    BevelMain: TSuperBevelPlus;
    BevelAnimation: TSuperBevelPlus;
    TimerFileDialog: TksTimer;
    TimerOpusApp: TksTimer;
    PrintButtonTimer: TksTimer;
    TimerAnimation: TksTimer;
    TimerHTMLBannerWeb: TksTimer;
    ButtonRefresh: TIBEAntialiasButton;
    ProgressBar: TALProgressBar;
    TimerIPC: TTimer;
    rkSmartTabs: TrkSmartTabs;
    ImageList1: TImageList;
    TimerErrorOccurred: TTimer;
    TimerCEFBrowserWindow: TTimer;
    TimerHideW81Start: TTimer;
    PgCtrl: TPageControl;

    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure ComboBoxLocationKeyPress(Sender: TObject; var Key: Char);
    procedure NavigateToComboBoxValue(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure ButtonBackClick(Sender: TObject);
    procedure ButtonForwardClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure ButtonRefreshClick(Sender: TObject);
    procedure ButtonZoomClick(Sender: TObject);
    procedure ButtonSearchClick(Sender: TObject);
    procedure ButtonPrintClick(Sender: TObject);
    procedure ButtonGoHomeClick(Sender: TObject);
    procedure WebBrowserHTMLBannerBeforeNavigate2(Sender: TObject;
      const pDisp: IDispatch; var URL, Flags, TargetFrameName, PostData,
      Headers: OleVariant; var Cancel: WordBool);
    {
    function DCEWebBrowserShowContextMenu(const dwID: Cardinal;
      const ppt: PPoint; const pcmdtReserved: IUnknown;
      const pdispReserved: IDispatch): HRESULT;
    function DCEWebBrowserTranslateAccelerator(const lpMsg: PMsg;
      const pguidCmdGroup: PGUID; const nCmdID: Cardinal): HRESULT;
    procedure DCEWebBrowserCloseQuery(Sender: TObject; var CanClose: Boolean);
    }
    procedure PanelHTMLBannerResize(Sender: TObject);
    procedure ButtonNewWindowClick(Sender: TObject);
    procedure TimerHTMLBannerWebTimer(Sender: TObject);
    procedure StatusBarDCEWebBrowserDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
    procedure TimerAnimationTimer(Sender: TObject);
    procedure DCEWebBrowserScriptError(Sender: TObject; ErrorLine,
      ErrorCharacter, ErrorCode, ErrorMessage, ErrorUrl: String;
      var ContinueScript, Showdialog: Boolean);
    procedure DCEWebBrowserWindowSetLeft(Sender: TObject; Left: Integer);
    procedure DCEWebBrowserWindowSetWidth(Sender: TObject; Width: Integer);
    procedure DCEWebBrowserWindowSetHeight(Sender: TObject; Height: Integer);
    procedure DCEWebBrowserWindowSetTop(Sender: TObject; Top: Integer);
    procedure TimerFileDialogTimer(Sender: TObject);
    procedure TimerOpusAppTimer(Sender: TObject);
    procedure PrintButtonTimerTimer(Sender: TObject);
    procedure TimerIPCTimer(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure rkSmartTabsAddClick(Sender: TObject);
    procedure rkSmartTabsCloseTab(Sender: TObject; Index: Integer;
      var Close: Boolean);
    procedure rkSmartTabsGetImageIndex(Sender: TObject; Tab: Integer;
      var Index: Integer);
    //procedure rkSmartTabsTabChange(Sender: TObject);
    procedure TimerErrorOccurredTimer(Sender: TObject);

    procedure ChrWebBrowserHTMLBannerWebLoadError(Sender: TObject; const browser: ICefBrowser;
      const frame: ICefFrame; errorCode: Integer; const errorText,
      failedUrl: ustring);

    procedure ChrWebBrowserHTMLBannerWebLoadEnd(Sender: TObject; const browser: ICefBrowser;
      const frame: ICefFrame; httpStatusCode: Integer);

    procedure ChrWebBrowserHTMLBannerWebBeforePopup(Sender: TObject; const browser: ICefBrowser;
      const frame: ICefFrame; const targetUrl, targetFrameName: ustring;
      var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
      var client: ICefClient; var settings: TCefBrowserSettings;
      var noJavascriptAccess: Boolean; out Result: Boolean);

    procedure WebBrowserNewWindow2(ASender: TObject;
      var ppDisp: IDispatch; var Cancel: WordBool);
    procedure ChrWebBrowserHTMLBannerWebBeforeMenu(Sender: TObject;
      const browser: ICefBrowser;
      out Result: Boolean);
    procedure ChrWebBrowserHTMLBannerWebAddressChange(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
    procedure TimerCEFBrowserWindowTimer(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure TimerHideW81StartTimer(Sender: TObject);
    procedure rkSmartTabsClick(Sender: TObject);
  protected
  private
    { Private declarations }
    fStatusBarHeight : integer;
    fWebBrowserStartPage: array[0..10] of String;
    fWebBrowserSearchPage: array[0..10] of String;
    fOtherApplicationOnlyAllowAccessTo: array[1..20] of Boolean;
    fOtherApplicationUseURLRestrictionsFromFile: array[1..20] of Boolean;
    fWebBrowserAllowTypedURLs : boolean;
    fWebBrowserFreeButAllowTypedURLs : boolean;
    IsfFreeClick : boolean;
    fSystemDefaultPrinter : integer;
    fPagesPrintedAllowed : integer;
    fCashCreditOrAccessCode : AnsiString;
    fCouponCode : string;
    fPrintingReceipt : boolean;
    fReceiptPrinter : AnsiString;
    fAllowWebPageToPrint : boolean;
    fUsageType : Integer;
    fWebBrowserFree : boolean;
    fToolBarWidth: Integer; // Contains width of all toolbar button items without the variable sized location combobox
    fNeedsToResetFontSize: Boolean;
    fClosing: Boolean;
    fHTMLBannerWebHeight: Integer;
    fHTMLBannerWebAddress: array[0..7] of OleVariant;
    fHTMLBannerWebEnglishAlwaysDisplayed: Boolean;
    fHTMLBannerWebReloadSeconds: Integer;
    fHTMLBannerCurrentURL: String;
    fLastReloadHTMLBannerWeb: Integer;
    fDownloadedHTMLBannerWebSuccessfully: Boolean;
    fBrowserDownloading: Boolean;
    FEnableParentOnClose: Boolean;
    FsCurrentURL: string;
    FsCurrentFileSpec: String;
    FIsClosing: boolean;
    FZooomLevel: integer;
    procedure WMMouseActivate(var Message: TMessage); message WM_MOUSEACTIVATE;
    procedure ReadConfiguration;
    procedure UpdateToolBarDimensions;
    procedure UpdateToolBarPositions;
    procedure SetStatusBar;
    procedure WMActivate(var Msg: TWMActivate); message WM_ACTIVATE;
    procedure SetsCurrentURL(const Value: string);
    procedure SetsCurrentFileSpec(const Value: String);
    procedure SetIsClosing(const Value: boolean);
    procedure SetZooomLevel(const Value: integer); // ACME 838
    procedure GetWBEXE;
    procedure ClearWBEXE;

  public
    CurrentTab : integer;
    iOtherAppIndex : integer;

    fWebBrowserRightClick: Boolean;

    //aWBFrame : array of TWBFrame;
    aTabRestrictions : array of string;
    //mh 1/11/2014
    aMainTabRestrictions : array of string;

    //aCRMPanel : array of TPanel;
    fKioskLanguage : integer;
    fPreviousKioskLanguage : integer;
    fKioskDefaultLanguage : integer;
    fWebBrowserAllowLocalDriveAccess : boolean;
    fWebBrowserRestrictPrintingMode : integer;
    fWebBrowserRestrictionMode : integer;
    fCurrentWebBrowserNavigationRestricted : boolean;
    fEnableFTP : boolean;
    fAccessibleDrives : string;
    fTempDirectoryVirtualDrive : string;
    StringListRestrictedURLsHTMLBannersAndOtherText : string;
    StringListRestrictedURLsText : string;
    StringListAllowToPrintURLs : TStringList;
    StringListRestrictedURLs : TstringList;
    StringListRestrictedURLsHTMLBannersAndOther : TstringList;
    lp : integer;
    //mh 6/22/2010 moved from private
    fPrintingEnabled: Boolean;
    //mh 6/21/2010
    PrintButtonTimerEnabled : boolean;

    function DisplayFNMessageForm(pMessageDisplayed: String; pButtonsDisplayed: Integer; pImageDisplayed: String; pDuration: Integer): Integer;

    procedure IPCSynchFromShellPro(Command: AnsiString);
    function GetTempDirectory: String;
    procedure SetLanguage;
    property EnableParentOnClose: Boolean read FEnableParentOnClose write FEnableParentOnClose;
    property sCurrentURL : string read FsCurrentURL write SetsCurrentURL;
    property sCurrentFileSpec : String read FsCurrentFileSpec write SetsCurrentFileSpec;
    property IsClosing : boolean read FIsClosing write SetIsClosing;
    property ZooomLevel : integer read FZooomLevel write SetZooomLevel;
    function GetTabName(sName : string) : string;
    procedure OpenNewTab(sNewURL : string);
    function TextFileIsUnicode(const TextFilename: string): boolean;
    procedure LoadUnicodeFile(const filename: String; strings: TStringList);
    procedure SwapWideChars(p: PWideChar);
    procedure LoadTextFile(const filename: string; strs: TStringList);
    //mh 5/16/2015
    function CheckUrl(url:string):boolean;

end;

const
  cToolBarSpacing: Integer = 5; // Vertical spacing between buttons
  cButtonsHeight: Integer = 42;
var
  FormDCEWebBrowser: TFormDCEWebBrowser;
  TempURL : string;
  sDefDir : String;
  sFileSpec : String;
  sUrl: string;
  sAdvURL : string;
  bUrlIsHome: boolean;
  sRestartURL : string;
  //aWBFrame : array of TWBFrame;

implementation

//uses Utilities, Constants, PrinterUtilities, Registry, MessageForm,
//     OtherLanguages, WinInet, FileManager, VersionConfig, FrameWB;

uses VersionConfig, Constants, Utilities, Registry, WinInet, PrinterUtilities,
     OtherLanguages, FrameWB, NSMessageForm;

{$R *.DFM}

//{$R administrator.RES}
// Text in different languages.
{$R ENGLISH.RES}
{$R FRENCH.RES}
{$R SPANISH.RES}
{$R GERMAN.RES}
{$R PORTUGUESE.RES}
{$R ITALIAN.RES}
{$R CHINESE.RES}

// Used to disable mouse wheel in combo box. Otherwise it navigates from page to page.
procedure TComboBox.ComboWndProc(var Message: TMessage; ComboWnd: HWnd; ComboProc: Pointer);
begin
 inherited;
 if (Focused and ((Message.Msg = WM_VSCROLL) or (Message.Msg = WM_HSCROLL) or (Message.msg = WM_Mousewheel))) then
 begin
   Message.Msg := 0;
 end;
end;

procedure TFormDCEWebBrowser.FormCreate(Sender: TObject);
var
  hHwnd : Hwnd;
  vlTS: TTabSheet;
  vlWBFrame: TWBFrame;
begin
  //mh 7/11/2014 - Samsung Tablet does not like the calls to the Log file
  //ShellProLog.DLog('TFormDCEWebBrowser.FormCreate - Enter');
  StringListAllowToPrintURLs := TStringList.Create;
  StringListRestrictedURLs := TStringList.Create;
  StringListRestrictedURLsHTMLBannersAndOther := TStringList.Create;

  IPCSynchFromShellPro('GetWBEXEPrintVars');

  //ChrWebBrowserHTMLBannerWeb.Left := 0;
  //ChrWebBrowserHTMLBannerWeb.Top := 0;

  // get the user default temp directory
  sDefDir := GetTempDirectory;
  if '' = sDefDir then
    sDefDir := GetDownloadFolderPath;

  sDefDir := IncludeTrailingPathDelimiter(sDefDir);

  FEnableParentOnClose := False;
  fBrowserDownloading := False;

  PanelAnimation.DoubleBuffered := True;

  // We keep track of when the web browser is closing to make sure we prevent
  // this closing from opening popups (see BeforeNavigate event).
  fClosing := False;

  // Reset the font size after the first document is loaded.
  fNeedsToResetFontSize := True;

  Left := 0;
  Top := 0;

  ReadConfiguration;
  SetLanguage;

  // Set the max height of the form. Required because of the status bar and
  // the on-screen keyboard.
  Constraints.MaxHeight := Screen.Height - fStatusBarHeight;
  Height := Constraints.MaxHeight;

  //Constraints.MaxWidth := FormMainMenu.GetApplicationWidth;
  Constraints.MaxWidth := screen.Width;

  Width := Constraints.MaxWidth;
  SetWindowLong( Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) and not WS_CAPTION) ;

  //GetWBEXE;

  if sURL = '' then
  begin
    sURL := sRestartURL;
  end;

  ComboBoxLocation.Text := sURL;
  ComboBoxLocation.Visible := fWebBrowserAllowTypedURLs and
                             (not fCurrentWebBrowserNavigationRestricted) and
                             (
                             (IsfFreeClick and fWebBrowserFreeButAllowTypedURLs) or
                             (not IsfFreeClick)
                             );
  BevelAnimation.Left := ClientWidth - (BevelAnimation.Width + cToolBarSpacing) + 2;
  PanelAnimation.Left := BevelAnimation.Left + ((BevelAnimation.Width - PanelAnimation.Width) div 2) + 2;
  UpdateToolBarDimensions;
  UpdateToolBarPositions;

  //Create new WB Frame to be placed on tab
  //CRMPanel.Align := alClient;

  //SetLength(aWBFrame,1);
  //SetLength(aCrmPanel,1);
  //SetLength(aTabCaptions,1);
  //SetLength(aTabRestrictions,1);
  //mh 1/11/2014
  //SetLength(aMainTabRestrictions,1);

  IPCSynchFromShellPro('GetURLRestrictions');
  //aTabRestrictions[0] := StringListRestrictedURLsHTMLBannersAndOther.Text;
  //StringListRestrictedURLsHTMLBannersAndOther.Text := aTabRestrictions[0];
  StringListRestrictedURLsHTMLBannersAndOther.Text :=  StringListRestrictedURLsHTMLBannersAndOtherText;

  //mh 1/11/2014
  IPCSynchFromShellPro('GetMainURLRestrictions');
  //aMainTabRestrictions[0] := StringListRestrictedURLs.Text;
  //StringListRestrictedURLs.Text := aMainTabRestrictions[0];
  StringListRestrictedURLs.Text := StringListRestrictedURLsText;


  //test
  //StringListRestrictedURLs.Add('*cnn*');
  //Application.MessageBox(pchar(StringListRestrictedURLsHTMLBannersAndOther.Text) ,'',mb_ok);
  //Application.MessageBox(pchar(StringListRestrictedURLs.Text) ,'',mb_ok);

  // If no URLs are listed as restrictions then the restriction mode is set
  // to 2, meaning no restriction.
  if StringListRestrictedURLs.Count < 1 then
    fWebBrowserRestrictionMode := 2;

  //mh 1/5/2014
  ShellProLog.DLog('----IPC From ShellPro -------------------------------------');
  ShellProLog.DLog('Response: GetURLRestrictions Banners and Other : ' + StringListRestrictedURLsHTMLBannersAndOtherText);
  ShellProLog.DLog('Response: GetMAINURLRestrictions : ' + StringListRestrictedURLsText);
  ShellProLog.DLog('-----------------------------------------------------------');

  //Application.MessageBox(pchar(StringListRestrictedURLsHTMLBannersAndOtherText),'',mb_ok);


   //mh 1/18/2014
  //aTabRestrictions[0] := StringListRestrictedURLsHTMLBannersAndOther.Text;

  //aCrmPanel[0] := TPanel.Create(FormDCEWebBrowser.CrmPanel);
  //aCrmPanel[0].Parent := CrmPanel;
  //aCrmPanel[0].Align := AlClient;

  //aWBFrame[0] := TWBFrame.Create(aCRMPanel[0]);
  //aWBFrame[0].Font.Name := CrmPanel.Font.Name;
  //aWBFrame[0].Font.Color := CrmPanel.Font.Color;
  //aWBFrame[0].Font.Size := CrmPanel.Font.Size;
  //aWBFrame[0].Font.Style := CrmPanel.Font.Style;
  //aWBFrame[0].Align := alClient;
  //aWBFrame[0].Parent := aCRMPanel[0];

  vlTS := TTabSheet.Create(PgCtrl);
  vlTS.Caption := ComboBoxLocation.Text;
  rkSmartTabs.AddTab(ComboBoxLocation.Text);
  vlTS.PageControl := PgCtrl;

  vlWBFrame := TWBFrame.Create(vlTS);
  vlWBFrame.Parent := vlTS;
  vlWBFrame.Align := alClient;

  //vlWBFrame.Chrm.Load('about.blank');
  PgCtrl.SelectNextPage(True,True);

  NavigateToComboBoxValue(Self);

  //rkSmartTabs.Tabs.Strings[0] := GetTabName(ComboBoxLocation.Text);

  // this keeps the CRMPanel from resizing properly
  //SetWindowPos(Handle, HWND_TOP, 0, 0, 0, 0, SWP_SHOWWINDOW Or SWP_NOMOVE Or SWP_NOSIZE);

  ClearWBEXE;
  TimerIPC.Enabled := True;

    //mh 1/30/2013 - Hide the Classic Shell Start Button if found (Windows 8)
  hHwnd := FindWindow(pChar('ClassicShell.CStartButton'), nil);
  if hHwnd > 0 then
  begin
    EnableWindow(hHwnd, FALSE);
    ShowWindow(hHwnd, SW_HIDE);
  end;
  //ShellProLog.DLog('TFormDCEWebBrowser.FormCreate - Exit');

  PgCtrl.Top := RKSmartTabs.Top;
  PgCtrl.Height := Height-PanelButtons.Height - statusbarWebBrowser.Height;
end;

procedure TFormDCEWebBrowser.FormActivate(Sender: TObject);
begin
  //mh 7/11/2014 - Samsung Tablet does not like the calls to the Log file
  //ShellProLog.DLog('TFormDCEWebBrowser.FormActivate - Enter');
  // Display the menu button on the status bar.
  //FormMainMenu.ButtonMenu.Visible := FormMainMenu.GetMenuButtonDisplayed;

  ButtonClose.Enabled := True;
  //ShellProLog.DLog('TFormDCEWebBrowser.FormActivate - Exit');
end;

procedure TFormDCEWebBrowser.FormShow(Sender: TObject);
begin
  //ShellProLog.DLog('TFormDCEWebBrowser.FormShow - Enter');

  if PanelButtons.Visible then
    SetStatusBar;

  //StringListRestrictedURLsHTMLBannersAndOther.Text := aTabRestrictions[rkSmartTabs.ActiveTab];
  //mh 1/11/2014
  //StringListRestrictedURLs.Text := aMainTabRestrictions[rkSmartTabs.ActiveTab];

  //Application.MessageBox(pchar(StringListRestrictedURLsHTMLBannersAndOther.Text) ,'',mb_ok);
  //Application.MessageBox(pchar(StringListRestrictedURLs.Text) ,'',mb_ok);


  //aWBFrame[rkSmartTabs.ActiveTab].Chrm.Load(ComboBoxLocation.Text);
  ShellProLog.DLog('TFormDCEWebBrowser.FormShow - Exit');
end;

function TFormDCEWebBrowser.GetTabName(sName: string): string;
var
  s : string;
begin
  if pos('.pdf',lowercase(sName)) > 0 then
  begin
    s := ExtractFileName(sName);
    s := StringReplace(s,'/','',[rfReplaceAll,rfIgnoreCase]);
  end else begin
    s := StringReplace(sName,'http://','',[rfReplaceAll,rfIgnoreCase]);
    s := StringReplace(s,'https://','',[rfReplaceAll,rfIgnoreCase]);
    s := StringReplace(s,'www.','',[rfReplaceAll,rfIgnoreCase]);
    if pos('/',s) > 0 then
      s := Copy(s,1,pos('/',s)-1);
  end;

  Result := s;
end;

function TFormDCEWebBrowser.GetTempDirectory: String;
begin
  Result := '';
  {$IFDEF DATSETTINGS}
  Result := Trim(LowerCase(NS_ReadString('TempFolder')));
  {$ELSE}
  with TRegistry.Create(KEY_READ) do
  begin
    try
      RootKey := MyRootKey;
      if OpenKey(NETSTOP_REG_KEY, FALSE) then
      begin
        Result := Trim(LowerCase(ReadString('TempFolder')));
        CloseKey;
      end;
    finally
      Free;
    end;
  end;
  {$ENDIF}
end;

procedure TFormDCEWebBrowser.GetWBEXE;
begin
  with TRegistry.Create(KEY_READ) do
  begin
    try
      Rootkey := MyRootKey;
      if OpenKey(NETSTOP_REG_KEY, FALSE) then
      begin
        sURL := ReadString('WBEXE - URL');
        bURLIsHome := ReadBool('WBEXE - IsHomeUrl');
        fCurrentWebBrowserNavigationRestricted := ReadBool('WBEXE - Restricted');
        IsfFreeClick := ReadBool('WBEXE - IsFreeClick');
        fSystemDefaultPrinter := ReadInteger('WBEXE - SysDefPrt');
        sRestartURL := ReadString('WBEXE - RestartURL');
        iOtherAppIndex := ReadInteger('WBEXE - OtherAppIndex');
        CloseKey;
      end;
    finally
      Free;
    end;
  end;
end;


procedure TFormDCEWebBrowser.IPCSynchFromShellPro(Command: AnsiString);
var
  Result: IIPCData;
  Request: IIPCData;
  IPCClient: TIPCClient;
begin
  //ShellProLog.DLog('TFormDCEWebBrowser.IPCSynchFromShellPro - ' + string(Command));

  //mh 1/5/2014
  //ShellProLog.DLog('----IPC From ShellPro -------------------------------------');
  //ShellProLog.DLog('Response: GetURLRestrictions 1: ' + StringListRestrictedURLsHTMLBannersAndOther.Text);
  //ShellProLog.DLog('Response: GetURLRestrictions 2: ' + StringListRestrictedURLsHTMLBannersAndOtherText);
  //ShellProLog.DLog('-----------------------------------------------------------');


  IPCClient := TIPCClient.Create;
  try
    IPCClient.ComputerName := '';
    IPCClient.ServerName := 'SPIPCSvr';
    IPCClient.ConnectClient(cDefaultTimeout);
    try
      if IPCClient.IsConnected then
      begin
        Request := AcquireIPCData;
        Request.ID := DateTimeToStr(Now);
        Request.Data.WriteUTF8String('Command', Command);
        Result := IPCClient.ExecuteConnectedRequest(Request);

        if IPCClient.AnswerValid then
        begin
          if Command = 'GetWBEXEPrintVars' then
          begin
            fPagesPrintedAllowed := Result.Data.ReadInteger('fPagesPrintedAllowed');
            fCashCreditOrAccessCode := Result.Data.ReadUTF8String('fCashCreditOrAccessCode');
            fCouponCode := string(Result.Data.ReadUTF8String('fCouponCode'));
            fPrintingReceipt := Result.Data.ReadBoolean('fPrintingReceipt');
            fReceiptPrinter := Result.Data.ReadUTF8String('fReceiptPrinter');
            fWebBrowserRestrictPrintingMode := Result.Data.ReadInteger('fWebBrowserRestrictPrintingMode');
            //fWebBrowserRestrictionMode := Result.Data.ReadInteger('fWebBrowserRestrictionMode');
            fAllowWebPageToPrint := Result.Data.ReadBoolean('fAllowWebPageToPrint');
            fUsageType := Result.Data.ReadInteger('fUsageType');
            fWebBrowserFree := Result.Data.ReadBoolean('fWebBrowserFree');
            fKioskLanguage := Result.Data.ReadInteger('fKioskLanguage');
            fPreviousKioskLanguage := Result.Data.ReadInteger('fPreviousKioskLanguage');

            //mh 2/27/2013 turn off the comments
            {
            ShellProLog.DLog('----IPC From ShellPro -------------------------------------');
            ShellProLog.DLog(Format('Synchronous Response ID: %s', [Result.ID]));
            ShellProLog.DLog(Format('Response: fPagesPrintedAllowed [%d]', [Result.Data.ReadInteger('fPagesPrintedAllowed')]));
            ShellProLog.DLog(Format('Response: fCashCreditOrAccessCode [%s]', [Result.Data.ReadUTF8String('fCashCreditOrAccessCode')]));
            ShellProLog.DLog(Format('Response: fCouponCode [%s]', [Result.Data.ReadUTF8String('fCouponCode')]));

            if Result.Data.ReadBoolean('fPrintingReceipt') then
              ShellProLog.DLog('Response: fPrintingReceipt [TRUE]')
            else
              ShellProLog.DLog('Response: fPrintingReceipt [FALSE]');

            ShellProLog.DLog(Format('Response: fReceiptPrinter [%s]', [Result.Data.ReadUTF8String('fReceiptPrinter')]));
            ShellProLog.DLog(Format('Response: fWebBrowserRestrictPrintingMode [%d]', [Result.Data.ReadInteger('fWebBrowserRestrictPrintingMode')]));
            ShellProLog.DLog(Format('Response: fWebBrowserRestrictionMode [%d]', [Result.Data.ReadInteger('fWebBrowserRestrictionMode')]));

            if Result.Data.ReadBoolean('fAllowWebPageToPrint') then
              ShellProLog.DLog('Response: fAllowWebPageToPrint [TRUE]')
            else
              ShellProLog.DLog('Response: fAllowWebPageToPrint [FALSE]');

            ShellProLog.DLog(Format('Response: fUsageType [%d]', [Result.Data.ReadInteger('fUsageType')]));
            ShellProLog.DLog(Format('Response: fKioskLanguage [%d]', [Result.Data.ReadInteger('fKioskLanguage')]));
            ShellProLog.DLog(Format('Response: fPreviousKioskLanguage [%d]', [Result.Data.ReadInteger('fPreviousKioskLanguage')]));

            //ShellProLog.DLog(Format('Response: Real [%f]', [Result.Data.ReadReal('Real')]));
            //ShellProLog.DLog(Format('Response: String [%s]', [Result.Data.ReadUTF8String('String')]));
            ShellProLog.DLog('-----------------------------------------------------------');
            }
          end;
          // if Command = 'CallFileManager' // has no return values
          // if Command = 'SetDisplayNotAllowPrintingMessageTrue'  // has no return values
          // if Command = 'SetAllowToPrintTrue'  // has no return values
          // if Command = 'SetAllowToPrintFalse'  // has no return values
          // if Command = 'ReEnableHTMLBanners'  // has no return values
          // if Command = 'ResetSecondsIdle'  //has no return values
          if Command = 'GetURLRestrictions' then
          begin
            StringListRestrictedURLsHTMLBannersAndOtherText := String(Result.Data.ReadUTF8String('StringListRestrictedURLsHTMLBannersAndOtherText'));
            //ShellProLog.DLog('----IPC From ShellPro -------------------------------------');
            //ShellProLog.DLog(Format('Response: GetURLRestrictions [%s]', [Result.Data.ReadUTF8String('StringListRestrictedURLsHTMLBannersAndOtherText')]));
            //ShellProLog.DLog('-----------------------------------------------------------');
          end;
          if Command = 'GetMainURLRestrictions' then
          begin
            StringListRestrictedURLsText := String(Result.Data.ReadUTF8String('StringListRestrictedURLsText'));
            //ShellProLog.DLog('----IPC From ShellPro -------------------------------------');
            //ShellProLog.DLog(Format('Response: GetURLRestrictions [%s]', [Result.Data.ReadUTF8String('StringListRestrictedURLsHTMLBannersAndOtherText')]));
            //ShellProLog.DLog('-----------------------------------------------------------');
          end;

        end
      end;

      if IPCClient.LastError <> 0 then
        ShellProLog.DLog(Format('Error: Code %d', [IPCClient.LastError]) + ' ' + IPCCLIENT.ErrorDesc);
    finally
      IPCClient.DisconnectClient;
    end;
  finally
    IPCClient.Free;
  end;
end;


procedure TFormDCEWebBrowser.LoadTextFile(const filename: string;
  strs: TStringList);
var
  ms: TMemoryStream;
begin
  ms:= TMemoryStream.Create;
  try
    ms.LoadFromFile( filename );
    strs.LoadFromStream(ms);
  finally
    ms.free;
  end;
end;

procedure TFormDCEWebBrowser.LoadUnicodeFile(const filename: String;
  strings: TStringList);
var
  ms: TMemoryStream;
  wc: WideChar;
  pWc: PWideChar;
begin
  ms:= TMemoryStream.Create;
  try
    ms.LoadFromFile( filename );
    ms.Seek( 0, soFromend );
    wc := #0000;
    ms.Write( wc, sizeof(wc));
    pWC := ms.Memory;
    if pWc^ = #$FEFF then // normal byte order mark
      Inc(pWc)
    else if pWc^ = #$FFFE then begin // byte order is big-endian
      SwapWideChars( pWc );
      Inc( pWc );
    end { If }
    else;  // no byte order mark
    strings.Text := WideChartoString( pWc );
  finally
    ms.free;
  end;
end;

function TFormDCEWebBrowser.CheckUrl(url: string): boolean;
var
 hSession, hfile: hInternet;
 dwindex,dwcodelen :dword;
 dwcode:array[1..20] of char;
 res : pchar;
 begin
   if pos('http://',lowercase(url))=0 then
      url := 'http://'+url;
   Result := false;
   hSession := InternetOpen('InetURL:/1.0',
        INTERNET_OPEN_TYPE_PRECONFIG,nil, nil, 0);
   if assigned(hsession) then
     begin
       hfile := InternetOpenUrl(
            hsession,
            pchar(url),
            nil,
            0,
            INTERNET_FLAG_RELOAD,
            0);
       dwIndex  := 0;
       dwCodeLen := 10;
       HttpQueryInfo(hfile, HTTP_QUERY_STATUS_CODE,
               @dwcode, dwcodeLen, dwIndex);
       res := pchar(@dwcode);
       result:= (res ='200') or (res ='302');
       if assigned(hfile) then
         InternetCloseHandle(hfile);
       InternetCloseHandle(hsession);
     end;

 end;


procedure TFormDCEWebBrowser.ChrWebBrowserHTMLBannerWebAddressChange(
  Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame;
  const url: ustring);
var
  i : integer;
begin
{
  if url <> sAdvUrl then
  begin
    i := high(aCrmPanel);
    if aCrmPanel[i] <> nil then
      aCrmPanel[i].Visible := False;
    rkSmartTAbs.Tabs.Add(GetTabName(url));
    OpenNewTab(url);
    ChrWebBrowserHTMLBannerWeb.Browser.GoBack;
  end;
  }
end;

procedure TFormDCEWebBrowser.ChrWebBrowserHTMLBannerWebBeforeMenu(
  Sender: TObject; const browser: ICefBrowser; out Result: Boolean);
begin
  Result := True;
end;


procedure TFormDCEWebBrowser.ChrWebBrowserHTMLBannerWebBeforePopup(Sender: TObject; const browser: ICefBrowser;
  const frame: ICefFrame; const targetUrl, targetFrameName: ustring;
  var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
  var client: ICefClient; var settings: TCefBrowserSettings;
  var noJavascriptAccess: Boolean; out Result: Boolean);
begin
  Result := True;
end;

procedure TFormDCEWebBrowser.ChrWebBrowserHTMLBannerWebLoadEnd(Sender: TObject; const browser: ICefBrowser;
  const frame: ICefFrame; httpStatusCode: Integer);
var
  vURLString : string;
begin

  //vURLString := ChrWebBrowserHTMLBannerWeb.Browser.MainFrame.Url;
  //mh
  ShellProLog.DLog('ChrWebBrowserHTMLBannerWebLoadEnd URL: ' + vURLString);

  if Frame.IsMain then
  begin
    // This event is required only when reloading the banner, not when navigating
    // from the banner.
    //ChrWebBrowserHTMLBannerWeb.OnLoadEnd := nil;

    //ChrWebBrowserHTMLBannerWeb.OnLoadStart := ChrWebBrowserHTMLBannerWebLoadStart;

    //PanelHTMLBanner.Height := fHTMLBannerWebHeight;
  end;

  //PgCtrl.Height := Height - StatusBarWebBrowser.Height - (Integer(PanelButtons.Visible) * PanelButtons.Height) - (Integer(PanelHTMLBanner.Visible) * PanelHTMLBanner.Height);
  //PgCtrl.Top := (Integer(PanelButtons.Visible) * PanelButtons.Height) + (Integer(PanelHTMLBanner.Visible) * PanelHTMLBanner.Height);

end;

procedure TFormDCEWebBrowser.ChrWebBrowserHTMLBannerWebLoadError(Sender: TObject; const browser: ICefBrowser;
  const frame: ICefFrame; errorCode: Integer; const errorText,
  failedUrl: ustring);
begin
  //Result := FALSE;
end;

procedure TFormDCEWebBrowser.ClearWBEXE;
begin
  with TRegistry.Create(KEY_ALL_ACCESS) do
  begin
    try
      Rootkey := MyRootKey;
      if OpenKey(NETSTOP_REG_KEY, FALSE) then
      begin
        WriteString('WBEXE - URL','');
        WriteBool('WBEXE - IsHomeUrl',False);
        //mh 1/5/2014
        //WriteBool('WBEXE - Restricted',False);

        WriteBool('WBEXE - IsFreeClick',False);
        CloseKey;
      end;
    finally
      Free;
    end;
  end;
end;


procedure TFormDCEWebBrowser.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  //mh 7/11/2014 - Samsung Tablet does not like the calls to the Log file
  //ShellProLog.DLog('TFormDCEWebBrowser.FormCloseQuery');

  //if ButtonClose.Enabled then
  //  ShellProLog.DLog('ButtonClose Enabled')
  //else
  //  ShellProLog.DLog('ButtonClose Disabled');

  // Some web sites trigger the opening of another window when closing. We don't
  // want to allow these to open - for one thing they cause problems when
  // the kiosk is preparing for the next user, plus they are a potential source
  // of problems. This flag is used in the OnNewWindow event to prevent the
  // opening of a new window.
  fClosing := True;

  //mh 4/9/2013 - do we need this?  Unexpecteds seem to happen around this point
  {
  try
    Application.ProcessMessages;
  except
  end;
  }

end;

procedure TFormDCEWebBrowser.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  //mh 7/11/2014 - Samsung Tablet does not like the calls to the Log file
  //ShellProLog.DLog('TFormDCEWebBrowser.FormClose');

  StringListAllowToPrintURLs.Clear;
  StringListRestrictedURLs.Clear;
  StringListRestrictedURLsHTMLBannersAndOther.Clear;

  TimerAnimation.Enabled := False;
  TimerFileDialog.Enabled := False;
  //mh 10/25/2010
  TimerHTMLBannerWeb.Enabled := False;
  PrintButtonTimer.Enabled := False;
//  TimerCEFBrowserWindow.Enabled := False;

  try
    Action := caFree;
    Isclosing := True;
  except
    ShellProLog.CreateLogEntry('ERR','Error Terminating DCE Web Browser',True);
  end;

end;

procedure TFormDCEWebBrowser.FormDestroy(Sender: TObject);
begin
  //mh 7/11/2014 - Samsung Tablet does not like the calls to the Log file
  IPCSynchFromShellPro('DCEWebBrowserClosing');
  //ShellProLog.DLog('TFormDCEWebBrowser: FormDestroy');
end;

procedure TFormDCEWebBrowser.FormMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  //mh 11/15/2012
  //IPCSynchFromShellPro('ResetSecondsIdle');
end;

procedure TFormDCEWebBrowser.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  //mh 11/15/2012
  //IPCSynchFromShellPro('ResetSecondsIdle');
end;

procedure TFormDCEWebBrowser.FormResize(Sender: TObject);
begin

end;

// This procedure is called when the focus is on the ComboBoxLocation and the
// user selects <ENTER>. It navigates to the URL in the ComboBoxLocation.
procedure TFormDCEWebBrowser.ComboBoxLocationKeyPress(Sender: TObject; var Key: Char);
var
  Cmp : TComponent;
  vlWBFrame: TWBFrame;
begin
  if Key = #13 then
  begin
    Key := #0;
    try
      {$IFNDEF TESTMODE}
      //StringListRestrictedURLsHTMLBannersAndOther.Text := aTabRestrictions[rkSmartTabs.ActiveTab];
      //mh 1/11/2014
      //StringListRestrictedURLs.Text := aMainTabRestrictions[rkSmartTabs.ActiveTab];
      {$ENDIF}
      //mh 5/16/2015
      //if CheckUrl(ComboBoxLocation.Text) then
      //  aWBFrame[rkSmartTabs.ActiveTab].Chrm.Load(ComboBoxLocation.Text)
      // else
      //  aWBFrame[rkSmartTabs.ActiveTab].Chrm.Load('http://www.google.com/search?q=' + ComboBoxLocation.Text);
      for Cmp in PgCtrl.ActivePage do
      begin
        if Cmp is TWBFrame then
        begin
          vlWBFrame := TWBFrame(Cmp);
          vlWBFrame.Chrm.Load(ComboBoxLocation.Text);
          rkSmartTabs.SetTabName(PgCtrl.ActivePageIndex,ComboBoxLocation.Text);
        end;
      end;
    except
      ShellProLog.DLog('ERR - TFormDCEWebBrowser.ComboBoxLocationKeyPress - ComboBoxLocation Error');
    end;
  end;
  //mh 11/15/2012
  //IPCSynchFromShellPro('ResetSecondsIdle');
end;



// This procedure is called when the user clicks on a URL in the ComboBoxLocation.
// It is also called when the user clicks on the Open button.
procedure TFormDCEWebBrowser.NavigateToComboBoxValue(Sender: TObject);
var
  Cmp : TComponent;
  vlWBFrame: TWBFrame;
begin
  // Navigate to the specified URL.
  try
    //StringListRestrictedURLsHTMLBannersAndOther.Text := aTabRestrictions[rkSmartTabs.ActiveTab];
    //mh 1/11/2014
    //StringListRestrictedURLs.Text := aMainTabRestrictions[rkSmartTabs.ActiveTab];

    for Cmp in PgCtrl.ActivePage do
    begin
      if Cmp is TWBFrame then
      begin
        vlWBFrame := TWBFrame(Cmp);
        vlWBFrame.Chrm.Load(ComboBoxLocation.Text);
        rkSmartTabs.SetTabName(PgCtrl.ActivePageIndex,ComboBoxLocation.Text);
      end;
    end;
  except
    ShellProLog.DLog('ERR - TFormDCEWebBrowser.NavigateToComboBoxValue - ComboBoxLocation Error');
  end;
end;

procedure TFormDCEWebBrowser.ButtonBackClick(Sender: TObject);
var
  Cmp : TComponent;
  vlWBFrame: TWBFrame;
begin
  for Cmp in PgCtrl.ActivePage do
  begin
    if Cmp is TWBFrame then
    begin
      vlWBFrame := TWBFrame(Cmp);
      if vlWBFrame.Chrm.Browser.CanGoBack then begin
        vlWBFrame.Chrm.Browser.GoBack;
        rkSmartTabs.SetTabName(PgCtrl.ActivePageIndex,vlWBFrame.Chrm.Browser.MainFrame.Name);
      end;
    end;
  end;
  ///StringListRestrictedURLsHTMLBannersAndOther.Text := aTabRestrictions[rkSmartTabs.ActiveTab];
  //mh 1/11/2014
  //StringListRestrictedURLs.Text := aMainTabRestrictions[rkSmartTabs.ActiveTab];
end;

procedure TFormDCEWebBrowser.ButtonForwardClick(Sender: TObject);
var
  Cmp : TComponent;
  vlWBFrame: TWBFrame;
begin
  for Cmp in PgCtrl.ActivePage do
  begin
    if Cmp is TWBFrame then
    begin
      vlWBFrame := TWBFrame(Cmp);
      if vlWBFrame.Chrm.Browser.CanGoForward then begin
        vlWBFrame.Chrm.Browser.GoForward;
        rkSmartTabs.SetTabName(PgCtrl.ActivePageIndex,vlWBFrame.Chrm.Browser.MainFrame.Name);
      end;
    end;
  end;
  //StringListRestrictedURLsHTMLBannersAndOther.Text := aTabRestrictions[rkSmartTabs.ActiveTab];
  //mh 1/11/2014
  //StringListRestrictedURLs.Text := aMainTabRestrictions[rkSmartTabs.ActiveTab];
end;

procedure TFormDCEWebBrowser.ButtonStopClick(Sender: TObject);
var
  Cmp : TComponent;
  vlWBFrame: TWBFrame;
begin
  for Cmp in PgCtrl.ActivePage do
  begin
    if Cmp is TWBFrame then
    begin
      vlWBFrame := TWBFrame(Cmp);
      vlWBFrame.Chrm.Browser.StopLoad;
    end;
  end;
end;

procedure TFormDCEWebBrowser.ButtonRefreshClick(Sender: TObject);
var
  Cmp : TComponent;
  vlWBFrame: TWBFrame;
begin
  for Cmp in PgCtrl.ActivePage do
  begin
    if Cmp is TWBFrame then
    begin
      vlWBFrame := TWBFrame(Cmp);
      vlWBFrame.Chrm.Browser.Reload;
    end;
  end;
end;

procedure TFormDCEWebBrowser.ButtonZoomClick(Sender: TObject);
var
  Cmp : TComponent;
  vlWBFrame: TWBFrame;
begin
  for Cmp in PgCtrl.ActivePage do
  begin
    if Cmp is TWBFrame then
    begin
      vlWBFrame := TWBFrame(Cmp);
    end;
  end;

  ZooomLevel := Zooomlevel + 1;
  //i := rkSmartTabs.ActiveTab;
  case ZooomLevel of
    0: begin
            vlWBFrame.Chrm.Browser.Host.ZoomLevel := 0;
       end;
    1 : begin
          vlWBFrame.Chrm.Browser.Host.ZoomLevel := vlWBFrame.Chrm.Browser.Host.ZoomLevel - 0.5;
        end;
    2,3,4 : begin
            vlWBFrame.Chrm.Browser.Host.ZoomLevel := vlWBFrame.Chrm.Browser.Host.ZoomLevel + 0.5;
          end;
  end;

  if ZooomLevel = 4 then
    ZooomLevel := -1;

end;

procedure TFormDCEWebBrowser.ButtonCloseClick(Sender: TObject);
var
  mrYes : Integer;
  //good : boolean;
begin
//mh 7/11/2014 - Samsung Tablet does not like the calls to the Log file
//ShellProLog.DLog('FormDCEWebBrowser - Enter ButtonCloseClick');

  //mh 4/17/2013
  //IPCSynchFromShellPro('PauseTimerDCEWBExists');

  //good := false;
  // close all?
  //while not good do
  //begin
    try
      if rkSmartTabs.Tabs.Count > 1 then
        mrYes := DisplayFNMessageForm(GetTextFromResourceOrRegistry(fKioskLanguage, 615), 5, 'Warning', 10000)
      else
        mrYes := DisplayFNMessageForm(GetTextFromResourceOrRegistry(fKioskLanguage, 694), 5, 'Warning', 10000);
      //good := True;
    except;

    end;
     //mrYes := Application.MessageBox(pChar(GetTextFromResourceOrRegistry(fKioskLanguage, 694)),'Warning',mb_YesNo);
 // end;

  //temp
  //mrYes := 6;

  //mh 4/17/2013
  //IPCSynchFromShellPro('ResumeTimerDCEWBExists');

  //mh 6/21/2013
  Sleep(10);

  //mh 1/11/2014
  ClearWBEXE;

  if (mrYes = 6) then
    Application.Terminate
  //mh 7/8/2013
  else
  begin
    IPCSynchFromShellPro('ReEnableHTMLBanners');
    //mh 6/21/2013
    Sleep(10);
  end;
end;


procedure TFormDCEWebBrowser.ButtonSearchClick(Sender: TObject);
var
  Cmp : TComponent;
  vlWBFrame: TWBFrame;
begin
  ComboBoxLocation.Text := fWebBrowserSearchPage[fKioskLanguage];
  for Cmp in PgCtrl.ActivePage do
  begin
    if Cmp is TWBFrame then
    begin
      vlWBFrame := TWBFrame(Cmp);
      vlWBFrame.Chrm.Load(ComboBoxLocation.Text);
      rkSmartTabs.SetTabName(PgCtrl.ActivePageIndex,ComboBoxLocation.Text);
    end;
  end;
end;


procedure TFormDCEWebBrowser.ButtonPrintClick(Sender: TObject);
var
  PrintingCoupon : Boolean;
  //i : integer;
  hHwnd,hHwnd2 : hWnd;
  sPrintURL : string;

  Cmp : TComponent;
  vlWBFrame: TWBFrame;
begin
  //mh 3/12/2012
  ShellProLog.DLog('FormDCEWebBrowser.ButtonPrint - Enter');
  ButtonPrint.Enabled := False;

  for Cmp in PgCtrl.ActivePage do
  begin
    if Cmp is TWBFrame then
      vlWBFrame := TWBFrame(Cmp);
  end;

  //i := rkSmartTabs.ActiveTab;

  sPrintURL := ComboBoxLocation.Text;

  ShellProLog.DLog('URLTOPRINT==> ' + sPrintURL );

  Printers.Printer.PrinterIndex := fSystemDefaultPrinter;
  SetPrinter;

  IPCSynchFromShellPro('GetWBEXEPrintVars');

  //mh 7/16/2012 - added pmPrepaidPages
  if not (MyVersionConfig.PaymentMode in [pmPrepaidPages, pmFreeWithTimer, pmFreeWithoutTimer, pmPayMinutesPagesUsed, pmPayMinutesPagesUsedWithAmount,pmFreeThenChargeMinutesPages,pmFreeThenChargeMinutesPagesAmount]) then
  begin
    if fPagesPrintedAllowed < 1 then
    begin
      //mh 2/10/2012

      if not (MyVersionConfig.PaymentMode in [pmPrepaidMinutesANdPagesEnterprise,pmPayMinutesPagesUsedEnterprise]) then
        DisplayFNMessageForm(GetTextFromResourceOrRegistry(fKioskLanguage, 704), 4, 'Error', 10000)
      else
        DisplayFNMessageForm(GetTextFromResourceOrRegistry(fKioskLanguage, 590), 4, 'Error', 10000);

      ButtonPrint.Enabled := fPrintingEnabled;
      EXIT;
    end;
  end;

  //mh 12/1/2010 - Used if Free Kiosk and no payment made yet but Pay for printing
  if fCashCreditOrAccessCode = 'x' then
  begin
    if fPagesPrintedAllowed < 1 then
    begin
      //mh 6/26/2012

      if (MyVersionConfig.PaymentMode in [pmFreeWithTimer, pmFreeWithoutTimer]) then
        DisplayFNMessageForm(GetTextFromResourceOrRegistry(fKioskLanguage, 704), 4, 'Error', 10000)
      else
        DisplayFNMessageForm(GetTextFromResourceOrRegistry(fKioskLanguage, 590), 4, 'Error', 10000);

      ButtonPrint.Enabled := fPrintingEnabled;
      EXIT;
    end;
  end;

  // COUPON PRINTING UPGRADE
  // Flag used to make sure the user is not charged to print a receipt.
  PrintingCoupon := False;
  if (Pos(uppercase(fCouponCode),Uppercase(GetActiveWindowTitle)) <> 0) then
  begin
    fPrintingReceipt := TRUE;
    PrintingCoupon := True;

    if (fReceiptPrinter <> 'Use Default Printer') then
      SetPrinterSpecific(String(fReceiptPrinter));
  end;

  Setcopies(0,false); // Issue 879 (4.8.2 Update Patch change)
  fOleAppIsPrinting := False;    // ACME 838
  ButtonPrint.Enabled := False;

  //Check for URLs allowed to print
  //mh 1/27/2011
  if ((fWebBrowserRestrictPrintingMode = 0) and (fAllowWebPageToPrint)) or
     ((fWebBrowserRestrictPrintingMode = 1) and (not fAllowWebPageToPrint)) then
  begin
    DisplayFNMessageForm(GetTextFromResourceOrRegistry(fKioskLanguage, 436), 4, 'Error', 5000);
    ButtonPrint.Enabled := fPrintingEnabled;
    Exit;
  end;

  try
    //mh 6/21/2010
    fPrintingEnabled := False;
    if (MyVersionConfig.PaymentMode = pmPrepaidMinutes) and (fUsageType = GetEnumValue(TypeInfo(TUsageType),'utAccessCode')) then
    begin
      // Confirm with the user that he wants to use his minutes to print.

      if ( DisplayFNMessageForm(GetTextFromResourceOrRegistry(fKioskLanguage, 431), 1, 'Confirmation', 0) = mrYes) then
      begin

        if vlWBFrame.Chrm.Browser <> nil then
        begin
            if pos('.pdf',lowercase(ComboBoxLocation.text)) > 0 then
            begin
              //navigate to handle of control
              hHwnd := vlWBFrame.Chrm.Handle; //CefBrowserWindow
              hHwnd := FindWindowEx(hHwnd, 0, pchar('WebViewHost'), nil);
              //ShellProLog.DLog('WebViewHost: ' + IntToStr(hHwnd));
              hHwnd := FindWindowEx(hHwnd, 0, pchar('NativeWindowClass'), nil);
              //ShellProLog.DLog('NativeWindowClass: ' + IntToStr(hHwnd));
              hHwnd := FindWindowEx(hHwnd, 0, pchar('Static'), nil);
              //ShellProLog.DLog('Static: ' + IntToStr(hHwnd));
              hHwnd := FindWindowEx(hHwnd, 0, pchar('AVL_AVView'), pchar('AVSplitterView'));
              //ShellProLog.DLog('AVSplitterView: ' + IntToStr(hHwnd));

              //if (not fIsWin8) then
              hHwnd2 := FindWindowEx(hHwnd, 0, pchar('AVL_AVView'), pchar('AVSplitterView'));
              if hHwnd2 > 0 then
              begin
               hHwnd := hHwnd2;
               ShellProLog.DLog('AVSplitterView: ' + IntToStr(hHwnd));
              end;

              hHwnd := FindWindowEx(hHwnd, 0, pchar('AVL_AVView'), pchar('AVSplitationPageView'));
              //ShellProLog.DLog('AVSplitationPageView: ' + IntToStr(hHwnd));
              hHwnd := FindWindowEx(hHwnd, 0, pchar('AVL_AVView'), pchar('AVSplitterView'));
              //ShellProLog.DLog('AVSplitterView: ' + IntToStr(hHwnd));
              hHwnd := FindWindowEx(hHwnd, 0, pchar('AVL_AVView'), pchar('AVScrolledPageView'));
              //ShellProLog.DLog('AVScrolledPageView: ' + IntToStr(hHwnd));
              hHwnd := FindWindowEx(hHwnd, 0, pchar('AVL_AVView'), pchar('AVScrollView'));
              //ShellProLog.DLog('AVScrollView: ' + IntToStr(hHwnd));
              hHwnd := FindWindowEx(hHwnd, 0, pchar('AVL_AVView'), pchar('AVPAgeView'));
              //ShellProLog.DLog('AVPAgeView: ' + IntToStr(hHwnd));

              Application.ProcessMessages;

              //bring to front
              SetForegroundWindow(hHwnd);

              //set focus by 'clicking' the page
              PostMessage(hHwnd, WM_LBUTTONDOWN, 0, 0);
              Sleep(1);
              PostMessage(hHwnd, WM_LBUTTONUP, 0, 0);
              Sleep(1);

              //send Ctrl-p to open the adobe print dialog
              keybd_event(VK_CONTROL, 1, 0, 0);
              keybd_event(VkKeyScan('P'), 1, 0, 0);
              keybd_event(VkKeyScan('P'), 1, KEYEVENTF_KEYUP, 0);
              keybd_event(VK_CONTROL, 1, KEYEVENTF_KEYUP, 0);
            end
            else
              vlWBFrame.Chrm.Browser.Host.Print;
        end;

      end

      else fPrintingEnabled := True;
    end else begin

      if vlWBFrame.Chrm.Browser <> nil then
      begin
          if pos('.pdf',lowercase(ComboBoxLocation.text)) > 0 then
          begin
            //navigate to handle of control

            hHwnd := vlWBFrame.Chrm.Handle; //CefBrowserWindow
            ShellProLog.DLog('CefBrowserWindow: ' + IntToStr(hHwnd));
            hHwnd := FindWindowEx(hHwnd, 0, pchar('WebViewHost'), nil);
            ShellProLog.DLog('WebViewHost: ' + IntToStr(hHwnd));
            hHwnd := FindWindowEx(hHwnd, 0, pchar('NativeWindowClass'), nil);
            ShellProLog.DLog('NativeWindowClass: ' + IntToStr(hHwnd));
            hHwnd := FindWindowEx(hHwnd, 0, pchar('Static'), nil);
            ShellProLog.DLog('Static: ' + IntToStr(hHwnd));
            hHwnd := FindWindowEx(hHwnd, 0, pchar('AVL_AVView'), pchar('AVSplitterView'));
            ShellProLog.DLog('AVSplitterView: ' + IntToStr(hHwnd));

            //if (not fIsWin8) then
            hHwnd2 := FindWindowEx(hHwnd, 0, pchar('AVL_AVView'), pchar('AVSplitterView'));
            if hHwnd2 > 0 then
            begin
             hHwnd := hHwnd2;
             ShellProLog.DLog('AVSplitterView: ' + IntToStr(hHwnd));
            end;

            hHwnd := FindWindowEx(hHwnd, 0, pchar('AVL_AVView'), pchar('AVSplitationPageView'));
            ShellProLog.DLog('AVSplitationPageView: ' + IntToStr(hHwnd));
            hHwnd := FindWindowEx(hHwnd, 0, pchar('AVL_AVView'), pchar('AVSplitterView'));
            ShellProLog.DLog('AVSplitterView: ' + IntToStr(hHwnd));
            hHwnd := FindWindowEx(hHwnd, 0, pchar('AVL_AVView'), pchar('AVScrolledPageView'));
            ShellProLog.DLog('AVScrolledPageView: ' + IntToStr(hHwnd));
            hHwnd := FindWindowEx(hHwnd, 0, pchar('AVL_AVView'), pchar('AVScrollView'));
            ShellProLog.DLog('AVScrollView: ' + IntToStr(hHwnd));
            hHwnd := FindWindowEx(hHwnd, 0, pchar('AVL_AVView'), pchar('AVPAgeView'));
            ShellProLog.DLog('AVPAgeView: ' + IntToStr(hHwnd));

            Application.ProcessMessages;

            //bring to front
            SetForegroundWindow(hHwnd);

            //set focus by 'clicking' the page
            PostMessage(hHwnd, WM_LBUTTONDOWN, 0, 0);
            Sleep(1);
            PostMessage(hHwnd, WM_LBUTTONUP, 0, 0);
            Sleep(1);

            //send Ctrl-p to open the adobe print dialog
            keybd_event(VK_CONTROL, 1, 0, 0);
            keybd_event(VkKeyScan('P'), 1, 0, 0);
            keybd_event(VkKeyScan('P'), 1, KEYEVENTF_KEYUP, 0);
            keybd_event(VK_CONTROL, 1, KEYEVENTF_KEYUP, 0);
          end
          else
            vlWBFrame.Chrm.Browser.Host.Print;
      end;
    end;

  finally
    if PrintingCoupon then
    begin
      fPrintingReceipt := FALSE;
      Printer.PrinterIndex := -1; // Reset to default printer
    end;

    Sleep(500);
    ShellProLog.DLog('FormDCEWebBrowser.ButtonPrint - Exit');
  end;

  ButtonPrint.Enabled := True;
end;

procedure TFormDCEWebBrowser.ButtonGoHomeClick(Sender: TObject);
var
  Cmp : TComponent;
  vlWBFrame: TWBFrame;
begin
  ShellProLog.DLog('FormDCEWebBrowser ButtonGoHomeClick');
  ComboBoxLocation.Text := fWebBrowserStartPage[fKioskLanguage];
  for Cmp in PgCtrl.ActivePage do
  begin
    if Cmp is TWBFrame then
    begin
      vlWBFrame := TWBFrame(Cmp);
      vlWBFrame.Chrm.Load(ComboBoxLocation.Text);
      rkSmartTabs.SetTabName(PgCtrl.ActivePageIndex,ComboBoxLocation.Text);
      //NavigateToComboBoxValue(Self);
    end;
  end;
end;


{$WRITEABLECONST OFF}

procedure TFormDCEWebBrowser.WebBrowserHTMLBannerBeforeNavigate2(Sender: TObject;
  const pDisp: IDispatch; var URL, Flags, TargetFrameName, PostData,
  Headers: OleVariant; var Cancel: WordBool);

begin
  // We open a new browser window so the current navigation is cancelled
  Cancel := True;
  OpenNewTab(URL);
end;

procedure TFormDCEWebBrowser.WebBrowserNewWindow2(ASender: TObject;
  var ppDisp: IDispatch; var Cancel: WordBool);
var
  i : integer;
begin
{
  i := high(aCrmPanel);
  if aCrmPanel[i] <> nil then
    aCrmPanel[i].Visible := False;
  rkSmartTAbs.Tabs.Add(GetTabName(fWebBrowserStartPage[fKioskLanguage]));
  OpenNewTab(fWebBrowserStartPage[fKioskLanguage]);
  }
end;

{
function TFormDCEWebBrowser.DCEWebBrowserShowContextMenu(const dwID: Cardinal;
  const ppt: PPoint; const pcmdtReserved: IUnknown;
  const pdispReserved: IDispatch): HRESULT;
begin
  if fWebBrowserRightClick then
    Result := S_FALSE
  else
    Result := S_OK;
end;
}
{
function TFormDCEWebBrowser.DCEWebBrowserTranslateAccelerator(const lpMsg: PMsg;
  const pguidCmdGroup: PGUID; const nCmdID: Cardinal): HRESULT;
begin
  if (GetKeystate(VK_CONTROL) < 0) and ((lpmsg.wParam = Ord('N')) or (lpmsg.wParam = Ord('P'))) then
    Result := S_OK
  else
    Result := S_FALSE;
end;
}
{
procedure TFormDCEWebBrowser.DCEWebBrowserCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FALSE;
end;
}

procedure TFormDCEWebBrowser.PanelHTMLBannerResize(Sender: TObject);
begin
  //ChrWebBrowserHTMLBannerWeb.Width := PanelHTMLBanner.Width;
  //ChrWebBrowserHTMLBannerWeb.Height := PanelHTMLBanner.Height;
end;

procedure TFormDCEWebBrowser.WMMouseActivate(var Message: TMessage);
begin
  Message.Result := MA_NOACTIVATE;
end;



procedure TFormDCEWebBrowser.OpenNewTab(sNewURL: string);
var
//  i:integer;
  vlTS: TTabSheet;
  vlWBFrame: TWBFrame;
begin
  //i := High(aWBFrame);
  //while aWBFrame[i] = nil do
  //  i := i - 1;

  //Create new WB Frame & TPanel
  //SetLength(aWBFrame,i + 2);
  //SetLength(aCRMPanel,i + 2);

  //aCRMPanel[i].Visible := False;
  //aCrmPanel[i+1] := TPanel.Create(FormDCEWebBrowser.CrmPanel);
  //aCrmPanel[i+1].Parent := CrmPanel;
  //aCrmPanel[i+1].Align := AlClient;

  //aWBFrame[i+1] := TWBFrame.Create(aCRMPanel[i+1]);
  //aWBFrame[i+1].Align := alClient;
  //aWBFrame[i+1].Parent := aCRMPanel[i+1];
  //aWBFrame[i+1].Chrm.Load(sNewURL);

  vlTS := TTabSheet.Create(PgCtrl);
  vlTS.Caption := sNewURL;
  rkSmartTabs.AddTab(sNewURL);
  vlTS.PageControl := PgCtrl;

  vlWBFrame := TWBFrame.Create(vlTS);
  vlWBFrame.Parent := vlTS;
  vlWBFrame.Align := alClient;
  //vlWBFrame.Chrm.Load('about.blank');
  PgCtrl.SelectNextPage(True,True);

  NavigateToComboBoxValue(self);


  ComboBoxLocation.Text := sNewURL;

  //rkSmartTabs.ActiveTab := i + 1;
end;

// This procedure sets the text to the current language as selected by the user.
// Strings of characters are retrieved from resource files.
procedure TFormDCEWebBrowser.SetLanguage;
begin
  // Normally we want to leave the caption since it is the web page title
  if Caption = '' then
    Caption := GetTextFromResourceOrRegistry(fKioskLanguage, 196);

  {$IFDEF DATSETTINGS}
  if NS_ReadBool('Application Web Browser Display Icon Only - Close') then
  begin
    ButtonClose.Caption := '';
    ButtonClose.Hint := GetTextFromResourceOrRegistry(fKioskLanguage, 268);
    ButtonClose.ShowHint := True;
    ButtonClose.Margins.Left := 4;
    ButtonClose.Margins.Right := 4;
  end
  else
  begin
    ButtonClose.Caption := GetTextFromResourceOrRegistry(fKioskLanguage, 268);
    ButtonClose.Hint := '';
    ButtonClose.ShowHint := False;
    ButtonClose.Margins.Left := 4;
    ButtonClose.Margins.Right := 8;
  end;
  if NS_ReadBool('Application Web Browser Display Icon Only - Back') then
  begin
    ButtonBack.Caption := '';
    ButtonBack.Hint := GetTextFromResourceOrRegistry(fKioskLanguage, 370);
    ButtonBack.ShowHint := True;
    ButtonBack.Margins.Left := 4;
    ButtonBack.Margins.Right := 4;
  end
  else
  begin
    ButtonBack.Caption := GetTextFromResourceOrRegistry(fKioskLanguage, 370);
    ButtonBack.Hint := '';
    ButtonClose.ShowHint := False;
    ButtonBack.Margins.Left := 4;
    ButtonBack.Margins.Right := 8;
  end;
  if NS_ReadBool('Application Web Browser Display Icon Only - Forward') then
  begin
    ButtonForward.Caption := '';
    ButtonForward.Hint := GetTextFromResourceOrRegistry(fKioskLanguage, 371);
    ButtonForward.ShowHint := True;
    ButtonForward.Margins.Left := 4;
    ButtonForward.Margins.Right := 4;
  end
  else
  begin
    ButtonForward.Caption := GetTextFromResourceOrRegistry(fKioskLanguage, 371);
    ButtonForward.Hint := '';
    ButtonForward.ShowHint := False;
    ButtonForward.Margins.Left := 4;
    ButtonForward.Margins.Right := 8;
  end;
  if NS_ReadBool('Application Web Browser Display Icon Only - Stop') then
  begin
    ButtonStop.Caption := '';
    ButtonStop.Hint := GetTextFromResourceOrRegistry(fKioskLanguage, 285);
    ButtonStop.ShowHint := True;
    ButtonStop.Margins.Left := 4;
    ButtonStop.Margins.Right := 4;
  end
  else
  begin
    ButtonStop.Caption := GetTextFromResourceOrRegistry(fKioskLanguage, 285);
    ButtonStop.Hint := '';
    ButtonStop.ShowHint := False;
    ButtonStop.Margins.Left := 4;
    ButtonStop.Margins.Right := 8;
  end;
  if NS_ReadBool('Application Web Browser Display Icon Only - Refresh') then
  begin
    ButtonRefresh.Caption := '';
    ButtonRefresh.Hint := GetTextFromResourceOrRegistry(fKioskLanguage, 372);
    ButtonRefresh.ShowHint := True;
    ButtonRefresh.Margins.Left := 4;
    ButtonRefresh.Margins.Right := 4;
  end
  else
  begin
    ButtonRefresh.Caption := GetTextFromResourceOrRegistry(fKioskLanguage, 372);
    ButtonRefresh.Hint := '';
    ButtonRefresh.ShowHint := False;
    ButtonRefresh.Margins.Left := 4;
    ButtonRefresh.Margins.Right := 8;
  end;
  if NS_ReadBool('Application Web Browser Display Icon Only - Zoom') then
  begin
    ButtonZoom.Caption := '';
    ButtonZoom.Hint := GetTextFromResourceOrRegistry(fKioskLanguage, 373);
    ButtonZoom.ShowHint := True;
    ButtonZoom.Margins.Left := 4;
    ButtonZoom.Margins.Right := 4;
  end
  else
  begin
    ButtonZoom.Caption := GetTextFromResourceOrRegistry(fKioskLanguage, 373);
    ButtonZoom.Hint := '';
    ButtonRefresh.ShowHint := False;
    ButtonZoom.Margins.Left := 4;
    ButtonZoom.Margins.Right := 8;
  end;
  if NS_ReadBool('Application Web Browser Display Icon Only - Print') then
  begin
    ButtonPrint.Caption := '';
    ButtonPrint.Hint := GetTextFromResourceOrRegistry(fKioskLanguage, 342);
    ButtonPrint.ShowHint := True;
    ButtonPrint.Margins.Left := 4;
    ButtonPrint.Margins.Right := 4;
  end
  else
  begin
    ButtonPrint.Caption := GetTextFromResourceOrRegistry(fKioskLanguage, 342);
    ButtonPrint.Hint := '';
    ButtonPrint.ShowHint := False;
    ButtonPrint.Margins.Left := 4;
    ButtonPrint.Margins.Right := 4;
  end;
  if NS_ReadBool('Application Web Browser Display Icon Only - Search') then
  begin
    ButtonSearch.Caption := '';
    ButtonSearch.Hint := GetTextFromResourceOrRegistry(fKioskLanguage, 374);
    ButtonSearch.ShowHint := True;
    ButtonSearch.Margins.Left := 4;
    ButtonSearch.Margins.Right := 4;
  end
  else
  begin
    ButtonSearch.Caption := GetTextFromResourceOrRegistry(fKioskLanguage, 374);
    ButtonSearch.Hint := '';
    ButtonSearch.ShowHint := False;
    ButtonSearch.Margins.Left := 4;
    ButtonSearch.Margins.Right := 8;
  end;
  if NS_ReadBool('Application Web Browser Display Icon Only - Go Home') then
  begin
    ButtonGoHome.Caption := '';
    ButtonGoHome.Hint := GetTextFromResourceOrRegistry(fKioskLanguage, 369);
    ButtonGoHome.ShowHint := True;
    ButtonGoHome.Margins.Left := 4;
    ButtonGoHome.Margins.Right := 4;
  end
  else
  begin
    ButtonGoHome.Caption := GetTextFromResourceOrRegistry(fKioskLanguage, 369);
    ButtonGoHome.Hint := '';
    ButtonGoHome.ShowHint := False;
    ButtonGoHome.Margins.Left := 4;
    ButtonGoHome.Margins.Right := 8;
  end;
  if NS_ReadBool('Application Web Browser Display Icon Only - Open') then
  begin
    ButtonOpen.Caption := '';
    ButtonOpen.Hint := GetTextFromResourceOrRegistry(fKioskLanguage, 368);
    ButtonOpen.ShowHint := True;
    ButtonOpen.Margins.Left := 4;
    ButtonOpen.Margins.Right := 4;
  end
  else
  begin
    ButtonOpen.Caption := GetTextFromResourceOrRegistry(fKioskLanguage, 368);
    ButtonOpen.Hint := '';
    ButtonOpen.ShowHint := False;
    ButtonOpen.Margins.Left := 4;
    ButtonOpen.Margins.Right := 8;
  end;
  if NS_ReadBool('Application Web Browser Display Icon Only - New Window') then
  begin
    ButtonNewWindow.Caption := '';
    ButtonNewWindow.Hint := GetTextFromResourceOrRegistry(fKioskLanguage, 375);
    ButtonNewWindow.ShowHint := True;
    ButtonNewWindow.Margins.Left := 4;
    ButtonNewWindow.Margins.Right := 4;
  end
  else
  begin
    ButtonNewWindow.Caption := GetTextFromResourceOrRegistry(fKioskLanguage, 375);
    ButtonNewWindow.Hint := '';
    ButtonOpen.ShowHint := False;
    ButtonNewWindow.Margins.Left := 4;
    ButtonNewWindow.Margins.Right := 8;
  end;

  {$ELSE}
  with TRegistry.Create(KEY_READ) do
  try
    RootKey := MyRootKey;
    if OpenKey(NETSTOP_REG_KEY, FALSE) then
    begin
      if ReadBool('Application Web Browser Display Icon Only - Close') then
      begin
        ButtonClose.Caption := '';
        ButtonClose.Hint := GetTextFromResourceOrRegistry(fKioskLanguage, 268);
        ButtonClose.ShowHint := True;
        ButtonClose.Margins.Left := 4;
        ButtonClose.Margins.Right := 4;
      end
      else
      begin
        ButtonClose.Caption := GetTextFromResourceOrRegistry(fKioskLanguage, 268);
        ButtonClose.Hint := '';
        ButtonClose.ShowHint := False;
        ButtonClose.Margins.Left := 4;
        ButtonClose.Margins.Right := 8;
      end;

      if ReadBool('Application Web Browser Display Icon Only - Back') then
      begin
        ButtonBack.Caption := '';
        ButtonBack.Hint := GetTextFromResourceOrRegistry(fKioskLanguage, 370);
        ButtonBack.ShowHint := True;
        ButtonBack.Margins.Left := 4;
        ButtonBack.Margins.Right := 4;
      end
      else
      begin
        ButtonBack.Caption := GetTextFromResourceOrRegistry(fKioskLanguage, 370);
        ButtonBack.Hint := '';
        ButtonClose.ShowHint := False;
        ButtonBack.Margins.Left := 4;
        ButtonBack.Margins.Right := 8;
      end;

      if ReadBool('Application Web Browser Display Icon Only - Forward') then
      begin
        ButtonForward.Caption := '';
        ButtonForward.Hint := GetTextFromResourceOrRegistry(fKioskLanguage, 371);
        ButtonForward.ShowHint := True;
        ButtonForward.Margins.Left := 4;
        ButtonForward.Margins.Right := 4;
      end
      else
      begin
        ButtonForward.Caption := GetTextFromResourceOrRegistry(fKioskLanguage, 371);
        ButtonForward.Hint := '';
        ButtonForward.ShowHint := False;
        ButtonForward.Margins.Left := 4;
        ButtonForward.Margins.Right := 8;
      end;

      if ReadBool('Application Web Browser Display Icon Only - Stop') then
      begin
        ButtonStop.Caption := '';
        ButtonStop.Hint := GetTextFromResourceOrRegistry(fKioskLanguage, 285);
        ButtonStop.ShowHint := True;
        ButtonStop.Margins.Left := 4;
        ButtonStop.Margins.Right := 4;
      end
      else
      begin
        ButtonStop.Caption := GetTextFromResourceOrRegistry(fKioskLanguage, 285);
        ButtonStop.Hint := '';
        ButtonStop.ShowHint := False;
        ButtonStop.Margins.Left := 4;
        ButtonStop.Margins.Right := 8;
      end;

      if ReadBool('Application Web Browser Display Icon Only - Refresh') then
      begin
        ButtonRefresh.Caption := '';
        ButtonRefresh.Hint := GetTextFromResourceOrRegistry(fKioskLanguage, 372);
        ButtonRefresh.ShowHint := True;
        ButtonRefresh.Margins.Left := 4;
        ButtonRefresh.Margins.Right := 4;
      end
      else
      begin
        ButtonRefresh.Caption := GetTextFromResourceOrRegistry(fKioskLanguage, 372);
        ButtonRefresh.Hint := '';
        ButtonRefresh.ShowHint := False;
        ButtonRefresh.Margins.Left := 4;
        ButtonRefresh.Margins.Right := 8;
      end;

      if ReadBool('Application Web Browser Display Icon Only - Zoom') then
      begin
        ButtonZoom.Caption := '';
        ButtonZoom.Hint := GetTextFromResourceOrRegistry(fKioskLanguage, 373);
        ButtonZoom.ShowHint := True;
        ButtonZoom.Margins.Left := 4;
        ButtonZoom.Margins.Right := 4;
      end
      else
      begin
        ButtonZoom.Caption := GetTextFromResourceOrRegistry(fKioskLanguage, 373);
        ButtonZoom.Hint := '';
        ButtonRefresh.ShowHint := False;
        ButtonZoom.Margins.Left := 4;
        ButtonZoom.Margins.Right := 8;
      end;

      if ReadBool('Application Web Browser Display Icon Only - Print') then
      begin
        ButtonPrint.Caption := '';
        ButtonPrint.Hint := GetTextFromResourceOrRegistry(fKioskLanguage, 342);
        ButtonPrint.ShowHint := True;
        ButtonPrint.Margins.Left := 4;
        ButtonPrint.Margins.Right := 4;
      end
      else
      begin
        ButtonPrint.Caption := GetTextFromResourceOrRegistry(fKioskLanguage, 342);
        ButtonPrint.Hint := '';
        ButtonPrint.ShowHint := False;
        ButtonPrint.Margins.Left := 4;
        ButtonPrint.Margins.Right := 4;
      end;

      if ReadBool('Application Web Browser Display Icon Only - Search') then
      begin
        ButtonSearch.Caption := '';
        ButtonSearch.Hint := GetTextFromResourceOrRegistry(fKioskLanguage, 374);
        ButtonSearch.ShowHint := True;
        ButtonSearch.Margins.Left := 4;
        ButtonSearch.Margins.Right := 4;
      end
      else
      begin
        ButtonSearch.Caption := GetTextFromResourceOrRegistry(fKioskLanguage, 374);
        ButtonSearch.Hint := '';
        ButtonSearch.ShowHint := False;
        ButtonSearch.Margins.Left := 4;
        ButtonSearch.Margins.Right := 8;
      end;

      if ReadBool('Application Web Browser Display Icon Only - Go Home') then
      begin
        ButtonGoHome.Caption := '';
        ButtonGoHome.Hint := GetTextFromResourceOrRegistry(fKioskLanguage, 369);
        ButtonGoHome.ShowHint := True;
        ButtonGoHome.Margins.Left := 4;
        ButtonGoHome.Margins.Right := 4;
      end
      else
      begin
        ButtonGoHome.Caption := GetTextFromResourceOrRegistry(fKioskLanguage, 369);
        ButtonGoHome.Hint := '';
        ButtonGoHome.ShowHint := False;
        ButtonGoHome.Margins.Left := 4;
        ButtonGoHome.Margins.Right := 8;
      end;

      if ReadBool('Application Web Browser Display Icon Only - Open') then
      begin
        ButtonOpen.Caption := '';
        ButtonOpen.Hint := GetTextFromResourceOrRegistry(fKioskLanguage, 368);
        ButtonOpen.ShowHint := True;
        ButtonOpen.Margins.Left := 4;
        ButtonOpen.Margins.Right := 4;
      end
      else
      begin
        ButtonOpen.Caption := GetTextFromResourceOrRegistry(fKioskLanguage, 368);
        ButtonOpen.Hint := '';
        ButtonOpen.ShowHint := False;
        ButtonOpen.Margins.Left := 4;
        ButtonOpen.Margins.Right := 8;
      end;

      if ReadBool('Application Web Browser Display Icon Only - New Window') then
      begin
        ButtonNewWindow.Caption := '';
        ButtonNewWindow.Hint := GetTextFromResourceOrRegistry(fKioskLanguage, 375);
        ButtonNewWindow.ShowHint := True;
        ButtonNewWindow.Margins.Left := 4;
        ButtonNewWindow.Margins.Right := 4;
      end
      else
      begin
        ButtonNewWindow.Caption := GetTextFromResourceOrRegistry(fKioskLanguage, 375);
        ButtonNewWindow.Hint := '';
        ButtonOpen.ShowHint := False;
        ButtonNewWindow.Margins.Left := 4;
        ButtonNewWindow.Margins.Right := 8;
      end;
    end;
  finally
    free;
  end;
  {$ENDIF}

  // This will trigger the reloading of the banner with the proper language if required.
  TimerHTMLBannerWeb.Period := 1000;

  UpdateToolBarDimensions;
  UpdateToolBarPositions;
end;

procedure TFormDCEWebBrowser.ButtonNewWindowClick(Sender: TObject);
var
  vlTS: TTabSheet;
  vlWBFrame: TWBFrame;
begin
  ShellProLog.DLog('ButtonNewWindow');
  vlTS := TTabSheet.Create(PgCtrl);
  ComboBoxLocation.Text := fWebBrowserStartPage[fKioskLanguage];
  vlTS.Caption := ComboBoxLocation.Text;
  rkSmartTabs.AddTab(ComboBoxLocation.Text);
  vlTS.PageControl := PgCtrl;
  PgCtrl.SelectNextPage(True,True);
  vlWBFrame := TWBFrame.Create(vlTS);
  vlWBFrame.Parent := vlTS;
  vlWBFrame.Align := alClient;
  vlWBFrame.Chrm.Load(ComboBoxLocation.Text);
  NavigateToComboBoxValue(Self);
end;


procedure TFormDCEWebBrowser.TimerHideW81StartTimer(Sender: TObject);
var
  hHwnd : Hwnd;
begin
  //mh 8/22/2014 - Try to Hide the Pesky ImmersiveSwitchList Start button (Windows 8.1)
  hHwnd := FindWindow(pChar('ImmersiveSwitchList'), pchar('Start'));
  if hHwnd > 0 then
  begin
    EnableWindow(hHwnd, FALSE);
    ShowWindow(hHwnd, SW_HIDE);
  end;
end;

procedure TFormDCEWebBrowser.TimerHTMLBannerWebTimer(Sender: TObject);
var
  vKioskLanguageTemp: Integer;
begin
  TimerHTMLBannerWeb.Period := fHTMLBannerWebReloadSeconds;

  if Application.Active and
     Active and
     ((Integer(GetTickCount) - fLastReloadHTMLBannerWeb) > fHTMLBannerWebReloadSeconds) or
      (fLastReloadHTMLBannerWeb = 0)
      or (fPreviousKioskLanguage <> fKioskLanguage) then
  begin
    try
      fLastReloadHTMLBannerWeb := GetTickCount;
      fDownloadedHTMLBannerWebSuccessfully := False;

      if fHTMLBannerWebEnglishAlwaysDisplayed then
        vKioskLanguageTemp := 0
      else
        vKioskLanguageTemp := fKioskLanguage;

      if IsHTTP(fHTMLBannerWebAddress[vKioskLanguageTemp]) or FileExists(Copy(fHTMLBannerWebAddress[vKioskLanguageTemp], 8, 1000)) then
      begin
        // The kiosk must be connected in order to load the banner - should always be when web browser opened anyway.
        //FormMainMenu.ProcessConnectionRequirement(False);

        // Prevent navigation into web browser
        //ChrWebBrowserHTMLBannerWeb.OnLoadStart := nil;
        //fHTMLBannerCurrentURL := fHTMLBannerWebAddress[vKioskLanguageTemp];
        //ChrWebBrowserHTMLBannerWeb.OnLoadEnd := ChrWebBrowserHTMLBannerWebLoadEnd;
        //ChrWebBrowserHTMLBannerWeb.Load(fHTMLBannerWebAddress[vKioskLanguageTemp]);

      end;
    except
    end;
  end;
end;

procedure TFormDCEWebBrowser.TimerIPCTimer(Sender: TObject);
begin
  GetWBEXE;
  //no action
  if sUrl = '' then Exit;

  //Bring app to top = do not navigate to startup
  if (NOT bURLIsHome) then
  begin
    ComboBoxLocation.Text := sURL;
    NavigateToComboBoxValue(ButtonOpen);
  end;

  SetWindowPos(Handle, HWND_TOP, 0, 0, 0, 0, SWP_SHOWWINDOW Or SWP_NOMOVE Or SWP_NOSIZE);

  ClearWBEXE;
end;

procedure TFormDCEWebBrowser.StatusBarDCEWebBrowserDrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
var
  vS: String;
begin
  StatusBar.Font.Style := [];
  StatusBar.Font.Name := Font.Name;
  StatusBar.Font.Color := Font.Color;

  with StatusBar.Canvas do
  begin
    vS := Panel.Text;

    // If the text is too long add '...'
    while ((Textwidth(vS + '...') > Panel.Width - 8) and (Length(vS) > 0)) do
    begin
      Delete(vS, Length(vS), 1);
    end;
    if Length(vS) < Length(Panel.Text) then
      vS := vS + '...';

    // Use attributes from setkiosk here - set all the font attributes maybe except the font size
    // to make sure it is small enough
    Brush.Color := Color;

    FillRect(Rect);

    // Center text vertically
    TextOut(Rect.left + 5, ((Rect.Bottom - Rect.Top - TextHeight(vS)) div 2) + 2, vS);
  end;
end;

procedure TFormDCEWebBrowser.SwapWideChars(p: PWideChar);
begin
  while p^ <> #0000 Do Begin
    //  p^ := Swap( p^ ); //<<<  D3
    p^ := WideChar( Swap( Word(p^)));
    Inc( p );
  end; { While }
end;

function TFormDCEWebBrowser.TextFileIsUnicode(
  const TextFilename: string): boolean;
var
  FS : TFileStream;
  w : word;
begin
  result := false;
  if FileExists(TextFilename) then begin
    FS := TFileStream.Create(TextFilename, fmOpenRead or fmShareDenyNone);
    try
      if FS.Read(w, SizeOf(w)) = SizeOf(w) then
        result := (w = $FEFF) or // normal byte order mark
                  (w = $FFFE);   // byte order is big-endian
    finally
      FS.Free;
    end;
  end;
end;


procedure TFormDCEWebBrowser.TimerAnimationTimer(Sender: TObject);
begin
  if ImageAnimation.Left = (-1 * ImageAnimation.Width) + (ImageAnimation.Width div 12) then
    ImageAnimation.Left := 0
  else
    ImageAnimation.Left := ImageAnimation.Left - (ImageAnimation.Width div 12);
  ImageAnimation.Repaint;
end;

procedure TFormDCEWebBrowser.TimerCEFBrowserWindowTimer(Sender: TObject);
var
  hHwnd : hwnd;
begin
  hHwnd := FindWindow(pchar('CEFBrowserWindow'),pchar(''));
  if hHwnd > 0 then
  begin
    if IsIconic(hHwnd) then
      ShowWindow(hHwnd,SW_SHOWNORMAL);
    BringWindowToTop(hHwnd);
  end;
end;

procedure TFormDCEWebBrowser.TimerErrorOccurredTimer(Sender: TObject);
var
  hHwndParent,hHwnd : hwnd;
begin
ShellProlog.DLog('TimerErrorOccurredTimer - Enter');

  hHwndParent := FindWindow(pchar('#32770'),pchar('Error occurred'));
  if hHwndParent = 0 then
    Exit;

  ShellProlog.DLog('FOUND #32770 ERROR OCCURRED');
  hHwnd := FindWindowEx(hHwndParent,0,pchar('Button'),pchar('Restart application'));
  if hHwnd <> 0 then
  begin
    ShellProlog.DLog('FOUND Button Restart Application');
    PostMessage(hHwnd, WM_LBUTTONDOWN, 0, 0);
    Sleep(1);
    PostMessage(hHwnd, WM_LBUTTONUP, 0, 0);
    Sleep(1);
  end;
  hHwnd := FindWindowEx(hHwndParent,0,pchar('Button'),pchar('&OK'));
  if hHwnd <> 0 then
  begin
    ShellProlog.DLog('FOUND Button &OK');
    keybd_event(VK_RETURN, 1, 0, 0);
    keybd_event(VK_RETURN, 1, KEYEVENTF_KEYUP, 0);
  end;
end;

procedure TFormDCEWebBrowser.ReadConfiguration;
var
  vButton: TIBEAntialiasButton;
  vIndex: Integer;
  vImageDirectory: String;
  vList : string;
begin

  GetWBEXE;

  vButton := nil;
  // Load images in buttons
  {$IFDEF DATSETTINGS}
  //mh 1/18/2014
  fWebBrowserRestrictionMode := NS_ReadInteger('Web Browser - Access');


  ButtonClose.Visible := NS_ReadBool('Application Web Browser Displayed - Close');
  ButtonZoom.Visible := NS_ReadBool('Application Web Browser Displayed - Zoom');
  ButtonSearch.Visible := NS_ReadBool('Application Web Browser Displayed - Search') and ComboBoxLocation.Visible;
  ButtonGoHome.Visible := NS_ReadBool('Application Web Browser Displayed - Go Home') and (not fCurrentWebBrowserNavigationRestricted);
  ButtonNewWindow.Visible := NS_ReadBool('Application Web Browser Displayed - New Window') and (not fCurrentWebBrowserNavigationRestricted) and (not IsfFreeClick);
  ButtonNewWindow.Visible := NS_ReadBool('Application Web Browser Displayed - New Window');

  ButtonBack.Visible := NS_ReadBool('Application Web Browser Displayed - Back');
  ButtonForward.Visible := NS_ReadBool('Application Web Browser Displayed - Forward');
  ButtonOpen.Visible :=  NS_ReadBool('Application Web Browser Displayed - Open');
  ButtonRefresh.Visible := NS_ReadBool('Application Web Browser Displayed - Refresh');
  ButtonStop.Visible := NS_ReadBool('Application Web Browser Displayed - Stop');

  ButtonPrint.Visible := NS_ReadBool('Application Web Browser Displayed - Print') and
                           MyVersionConfig.PrinterAvailable and
                           ((fUsageType in [ord(utAccessCode), ord(utFree), ord(utFreeApplication), ord(utDatabaseUnavailable), ord(utPayMinutesPagesUsed), ord(utCyberCafe), ord(utFreeThenCharge)])
                            or fWebBrowserFree);

  // Setup the download animation
  if FileExists(ExtractFilePath(Application.ExeName) + 'Interface\Animations\Globe30.png') then
    ImageAnimation.Picture.LoadFromFile(ExtractFilePath(Application.ExeName) + 'Interface\Animations\Globe30.png');

  BevelAnimation.Top := 5;
  PanelAnimation.Top := BevelAnimation.Top + ((BevelAnimation.Height - ImageAnimation.Height) div 2);
  PanelAnimation.Height := ImageAnimation.Height;
  PanelAnimation.Width := ImageAnimation.Width div 12;
  ImageAnimation.Top := 0;
  ImageAnimation.Left := 0;

  vImageDirectory := ExtractFilePath(Application.ExeName) + 'Interface\Applications\';

  if FileExists(vImageDirectory + 'Close.png') then
    ButtonClose.Glyph.LoadFromFile(vImageDirectory + 'Close.png');
  if FileExists(vImageDirectory + 'WebBrowserBack.png') then
    ButtonBack.Glyph.LoadFromFile(vImageDirectory + 'WebBrowserBack.png');
  if FileExists(vImageDirectory + 'WebBrowserForward.png') then
    ButtonForward.Glyph.LoadFromFile(vImageDirectory + 'WebBrowserForward.png');
  if FileExists(vImageDirectory + 'WebBrowserStop.png') then
    ButtonStop.Glyph.LoadFromFile(vImageDirectory + 'WebBrowserStop.png');
  if FileExists(vImageDirectory + 'WebBrowserRefresh.png') then
    ButtonRefresh.Glyph.LoadFromFile(vImageDirectory + 'WebBrowserRefresh.png');
  if FileExists(vImageDirectory + 'WebBrowserZoom.png') then
    ButtonZoom.Glyph.LoadFromFile(vImageDirectory + 'WebBrowserZoom.png');
  if FileExists(vImageDirectory + 'Print.png') then
    ButtonPrint.Glyph.LoadFromFile(vImageDirectory + 'Print.png');
  if FileExists(vImageDirectory + 'WebBrowserSearch.png') then
    ButtonSearch.Glyph.LoadFromFile(vImageDirectory + 'WebBrowserSearch.png');
  if FileExists(vImageDirectory + 'WebBrowserGoHome.png') then
    ButtonGoHome.Glyph.LoadFromFile(vImageDirectory + 'WebBrowserGoHome.png');
  if FileExists(vImageDirectory + 'WebBrowserOpen.png') then
    ButtonOpen.Glyph.LoadFromFile(vImageDirectory + 'WebBrowserOpen.png');
  if FileExists(vImageDirectory + 'WebBrowserNew.png') then
    ButtonNewWindow.Glyph.LoadFromFile(vImageDirectory + 'WebBrowserNew.png');

  Color := NS_ReadInteger('Application Background - Color');

  if NS_ReadBool('Application Web Browser Button Bar - Displayed') then
  begin
    PanelButtons.Visible := True;

    for vIndex := 1 to 11 do
    begin
      case vIndex of
        1: vButton := ButtonClose;
        2: vButton := ButtonBack;
        3: vButton := ButtonForward;
        4: vButton := ButtonStop;
        5: vButton := ButtonRefresh;
        6: vButton := ButtonZoom;
        7: vButton := ButtonSearch;
        8: vButton := ButtonNewWindow;
        9: vButton := ButtonPrint;
        10: vButton := ButtonOpen;
        11: vButton := ButtonGoHome;
      end;

      with vButton do
      begin
        Height := cButtonsHeight;
        BackColor := NS_ReadInteger('Application Background - Color');
        BorderSize := NS_ReadInteger('Application Buttons - Border Size');

        CornerRound := NS_ReadInteger('Application Buttons - Rounded Value');
        Corners.TopLeft := NS_ReadBool('Application Buttons - Rounded Top Left');
        Corners.TopRight  := NS_ReadBool('Application Buttons - Rounded Top Right');
        Corners.BottomLeft := NS_ReadBool('Application Buttons - Rounded Bottom Left');
        Corners.BottomRight := NS_ReadBool('Application Buttons - Rounded Bottom Right');
        GlyphAutoGray := NS_ReadBool('Application Buttons - Gray Icons');

        Font.Name := NS_ReadString('Application Buttons - Font Name');
        Font.Color := NS_ReadInteger('Application Buttons - Font Color');
        Font.Size := NS_ReadInteger('Application Buttons - Font Size');
        Font.Style := [];
        if NS_ReadBool('Application Buttons - Bold') then
          Font.Style := [fsBold];
        if NS_ReadBool('Application Buttons - Italic') then
          Font.Style := Font.Style + [fsItalic];
        if NS_ReadBool('Application Buttons - Underline') then
          Font.Style := Font.Style + [fsUnderline];

        Properties.Disabled.Color := NS_ReadInteger('Application Buttons - Disabled Color');
        Properties.Disabled.Border := NS_ReadInteger('Application Buttons - Disabled Color Border');
        Properties.Disabled.Font.Name := NS_ReadString('Application Buttons - Font Name');
        Properties.Disabled.Font.Color := NS_ReadInteger('Application Buttons - Disabled Color Font');
        Properties.Disabled.Font.Size := NS_ReadInteger('Application Buttons - Font Size');
        Properties.Disabled.Font.Style := [];
        if NS_ReadBool('Application Buttons - Bold') then
          Properties.Disabled.Font.Style := [fsBold];
        if NS_ReadBool('Application Buttons - Italic') then
          Properties.Disabled.Font.Style := Properties.Disabled.Font.Style + [fsItalic];
        if NS_ReadBool('Application Buttons - Underline') then
          Properties.Disabled.Font.Style := Properties.Disabled.Font.Style + [fsUnderline];

        Properties.MouseAway.Color := NS_ReadInteger('Application Buttons - Color');
        Properties.MouseAway.Border := NS_ReadInteger('Application Buttons - Border Color');
        Properties.MouseAway.Font.Name := NS_ReadString('Application Buttons - Font Name');
        Properties.MouseAway.Font.Color := NS_ReadInteger('Application Buttons - Font Color');
        Properties.MouseAway.Font.Size := NS_ReadInteger('Application Buttons - Font Size');
        Properties.MouseAway.Font.Style := [];
        if NS_ReadBool('Application Buttons - Bold') then
          Properties.MouseAway.Font.Style := [fsBold];
        if NS_ReadBool('Application Buttons - Italic') then
          Properties.MouseAway.Font.Style := Properties.MouseAway.Font.Style + [fsItalic];
        if NS_ReadBool('Application Buttons - Underline') then
          Properties.MouseAway.Font.Style := Properties.MouseAway.Font.Style + [fsUnderline];

        Properties.MouseDown.Color := NS_ReadInteger('Application Buttons - Mouse Down Color');
        Properties.MouseDown.Border := NS_ReadInteger('Application Buttons - Mouse Down Color Border');
        Properties.MouseDown.Font.Name := NS_ReadString('Application Buttons - Font Name');
        Properties.MouseDown.Font.Color := NS_ReadInteger('Application Buttons - Mouse Down Color Font');
        Properties.MouseDown.Font.Size := NS_ReadInteger('Application Buttons - Font Size');
        Properties.MouseDown.Font.Style := [];
        if NS_ReadBool('Application Buttons - Bold') then
          Properties.MouseDown.Font.Style := [fsBold];
        if NS_ReadBool('Application Buttons - Italic') then
          Properties.MouseDown.Font.Style := Properties.MouseDown.Font.Style + [fsItalic];
        if NS_ReadBool('Application Buttons - Underline') then
          Properties.MouseDown.Font.Style := Properties.MouseDown.Font.Style + [fsUnderline];

        Properties.MouseOver.Color := NS_ReadInteger('Application Buttons - Mouse Over Color');
        Properties.MouseOver.Border := NS_ReadInteger('Application Buttons - Mouse Over Color Border');
        Properties.MouseOver.Font.Name := NS_ReadString('Application Buttons - Font Name');
        Properties.MouseOver.Font.Color := NS_ReadInteger('Application Buttons - Mouse Over Color Font');
        Properties.MouseOver.Font.Size := NS_ReadInteger('Application Buttons - Font Size');
        Properties.MouseOver.Font.Style := [];
        if NS_ReadBool('Application Buttons - Bold') then
          Properties.MouseOver.Font.Style := [fsBold];
        if NS_ReadBool('Application Buttons - Italic') then
          Properties.MouseOver.Font.Style := Properties.MouseOver.Font.Style + [fsItalic];
        if NS_ReadBool('Application Buttons - Underline') then
          Properties.MouseOver.Font.Style := Properties.MouseOver.Font.Style + [fsUnderline];

      end;
    end;
  end;
  // Banner
  {
  PanelHTMLBanner.Visible := NS_ReadBool('HTML Banner (Web) - Displayed');
  fHTMLBannerWebHeight := NS_ReadInteger('HTML Banner (Web) - Height');
  PanelHTMLBanner.Height := 0;
  fHTMLBannerWebAddress[0] := NS_ReadString('HTML Banner (Web) - Source Prefix English') + NS_ReadString('HTML Banner (Web) - Source English');
  fHTMLBannerWebAddress[1] := NS_ReadString('HTML Banner (Web) - Source Prefix French') + NS_ReadString('HTML Banner (Web) - Source French');
  fHTMLBannerWebAddress[2] := NS_ReadString('HTML Banner (Web) - Source Prefix Spanish') + NS_ReadString('HTML Banner (Web) - Source Spanish');
  fHTMLBannerWebAddress[3] := NS_ReadString('HTML Banner (Web) - Source Prefix German') + NS_ReadString('HTML Banner (Web) - Source German');
  fHTMLBannerWebAddress[4] := NS_ReadString('HTML Banner (Web) - Source Prefix Portuguese') + NS_ReadString('HTML Banner (Web) - Source Portuguese');
  fHTMLBannerWebAddress[5] := NS_ReadString('HTML Banner (Web) - Source Prefix Italian') + NS_ReadString('HTML Banner (Web) - Source Italian');
  fHTMLBannerWebAddress[6] := NS_ReadString('HTML Banner (Web) - Source Prefix Other 1') + NS_ReadString('HTML Banner (Web) - Source Other 1');
  fHTMLBannerWebAddress[7] := NS_ReadString('HTML Banner (Web) - Source Prefix Other 2') + NS_ReadString('HTML Banner (Web) - Source Other 2');
  fHTMLBannerWebEnglishAlwaysDisplayed := NS_ReadBool('HTML Banner (Web) - English Always Displayed');
  fHTMLBannerWebReloadSeconds := NS_ReadInteger('HTML Banner (Web) - Reload Seconds') * 1000;
  }
  {$IFDEF TESTMODE}
  {
  PanelHTMLBanner.Visible := True;
  fHTMLBannerWebHeight := 60;
  PanelHTMLBanner.Height := 60;
  fHTMLBannerWebAddress[0] := 'http://www.kioskremote.net/secure/files/display.asp?ZoneID=85';
  fHTMLBannerWebEnglishAlwaysDisplayed := True;
  fHTMLBannerWebReloadSeconds := 15000;
  }
  {$ENDIF}

  // use this to work the adv banner
  sADVUrl := fHTMLBannerWebAddress[fKioskDefaultLanguage];

  // Text
  Font.Name := NS_ReadString('Application Text - Font Name');
  Font.Color := NS_ReadInteger('Application Text - Font Color');
  Font.Size := NS_ReadInteger('Application Text - Font Size');
  Font.Style := [];
  if NS_ReadBool('Application Text - Bold') then
    Font.Style := [fsBold];
  if NS_ReadBool('Application Text - Italic') then
    Font.Style := Font.Style + [fsItalic];
  if NS_ReadBool('Application Text - Underline') then
    Font.Style := Font.Style + [fsUnderline];

  //CRMPanel.Font.Name := NS_ReadString('Application Text - Font Name');
  //CRMPanel.Font.Color := NS_ReadInteger('Application Text - Font Color');
  //CRMPanel.Font.Size := NS_ReadInteger('Application Text - Font Size');
  //CRMPanel.Font.Style := [];

  fStatusBarHeight := NS_ReadInteger('Status Bar - Height');
  fKioskDefaultLanguage := NS_ReadInteger('Languages - Default Language');
  // Web Browser

  fWebBrowserStartPage[0] := NS_ReadString('Web Browser - Start Page English Prefix') +
                             NS_ReadString('Web Browser - Start Page English');
  fWebBrowserStartPage[1] := NS_ReadString('Web Browser - Start Page French Prefix') +
                             NS_ReadString('Web Browser - Start Page French');
  fWebBrowserStartPage[2] := NS_ReadString('Web Browser - Start Page Spanish Prefix') +
                             NS_ReadString('Web Browser - Start Page Spanish');
  fWebBrowserStartPage[3] := NS_ReadString('Web Browser - Start Page German Prefix') +
                             NS_ReadString('Web Browser - Start Page German');
  fWebBrowserStartPage[4] := NS_ReadString('Web Browser - Start Page Portuguese Prefix') +
                             NS_ReadString('Web Browser - Start Page Portuguese');
  fWebBrowserStartPage[5] := NS_ReadString('Web Browser - Start Page Italian Prefix') +
                             NS_ReadString('Web Browser - Start Page Italian');
  fWebBrowserStartPage[6] := NS_ReadString('Web Browser - Start Page OtherLanguage1 Prefix') +
                             NS_ReadString('Web Browser - Start Page OtherLanguage1');
  fWebBrowserStartPage[7] := NS_ReadString('Web Browser - Start Page OtherLanguage2 Prefix') +
                             NS_ReadString('Web Browser - Start Page OtherLanguage2');
  fWebBrowserStartPage[8] := '';
  fWebBrowserStartPage[9] := '';
  fWebBrowserStartPage[10] := NS_ReadString('Web Browser - Start Page Chinese Prefix') +
                             NS_ReadString('Web Browser - Start Page Chinese');


  fWebBrowserSearchPage[0] := NS_ReadString('Web Browser - Search Page English Prefix') +
                              NS_ReadString('Web Browser - Search Page English');
  fWebBrowserSearchPage[1] := NS_ReadString('Web Browser - Search Page French Prefix') +
                              NS_ReadString('Web Browser - Search Page French');
  fWebBrowserSearchPage[2] := NS_ReadString('Web Browser - Search Page Spanish Prefix') +
                              NS_ReadString('Web Browser - Search Page Spanish');
  fWebBrowserSearchPage[3] := NS_ReadString('Web Browser - Search Page German Prefix') +
                              NS_ReadString('Web Browser - Search Page German');
  fWebBrowserSearchPage[4] := NS_ReadString('Web Browser - Search Page Portuguese Prefix') +
                              NS_ReadString('Web Browser - Search Page Portuguese');
  fWebBrowserSearchPage[5] := NS_ReadString('Web Browser - Search Page Italian Prefix') +
                              NS_ReadString('Web Browser - Search Page Italian');
  fWebBrowserSearchPage[6] := NS_ReadString('Web Browser - Search Page OtherLanguage1 Prefix') +
                              NS_ReadString('Web Browser - Search Page OtherLanguage1');
  fWebBrowserSearchPage[7] := NS_ReadString('Web Browser - Search Page OtherLanguage2 Prefix') +
                              NS_ReadString('Web Browser - Search Page OtherLanguage2');
  fWebBrowserSearchPage[8] := '';
  fWebBrowserSearchPage[9] := '';
  fWebBrowserSearchPage[10] := NS_ReadString('Web Browser - Search Page Chinese Prefix') +
                              NS_ReadString('Web Browser - Search Page Chinese');
  fWebBrowserRightClick := NS_ReadBool('Web Browser - Right Click');

  for vIndex := 1 to 20 do
  begin
    fOtherApplicationOnlyAllowAccessTo[vIndex] := NS_ReadBool('Other Applications ' + IntToStr(vIndex) + ' - Only Allow Access To');
    fOtherApplicationUseURLRestrictionsFromFile[vIndex] := NS_ReadBool('Other Applications ' + IntToStr(vIndex) + ' - Load URL Restrictions From File');
  end;

  fWebBrowserAllowTypedURLs := NS_ReadBool('Web Browser - Allow Typed URLs');
  fWebBrowserFreeButAllowTypedURLs := NS_ReadBool('Web Browser - Free But Allow Typed URLs');
  fWebBrowserAllowLocalDriveAccess := NS_ReadBool('Web Browser - Allow Local Drive Access');

  vList := NS_ReadString('Web Browser - Allow Printing URLs');
  while Pos('&', vList) <> 0 do
  begin
    StringListAllowToPrintURLs.Add(copy(vList, 1, Pos('&', vList) - 1));
    vList := LowerCase(copy(vList, Pos('&', vList) + 1, Length(vList) - Pos('&', vList)));
  end;

  vList := NS_ReadString('Web Browser');
  while Pos('&', vList) <> 0 do
  begin
    StringListRestrictedURLsHTMLBannersAndOther.Add(copy(vList, 1, Pos('&', vList) - 1));
    vList := LowerCase(copy(vList, Pos('&', vList) + 1, Length(vList) - Pos('&', vList)));
  end;


  //mh 1/11/2014 - add other restrictions
  // load restricted URLs From File if exists

  if iOtherAppIndex = 0 then
  begin
    if NS_ReadBool('Web Browser - Load URL Restrictions From File') then
    begin
      //mh 1/11/2014
      //fWebBrowserRestrictionMode := 0;
      vList := NS_ReadString('Web Browser - URL Restrictions Filename');
      Application.ProcessMessages;
      if (vList <> '') and (FileExists(vList)) then
      begin
        if TextFileIsUnicode(vList) then
          LoadUnicodeFile( vList, StringListRestrictedURLs)
        else
          LoadTextFile( vList, StringListRestrictedURLs);
      end;
    end;
  end else begin
    if NS_ReadBool('Other Applications ' + IntToStr(vIndex) + ' - Load URL Restrictions From File') then
    begin
      vList := NS_ReadString('Other Applications ' + IntToStr(vIndex) + ' - URL Restrictions Filename');
      Application.ProcessMessages;
      if (vList <> '') and (FileExists(vList)) then
      begin
        if TextFileIsUnicode(vList) then
          LoadUnicodeFile( vList, StringListRestrictedURLs)
        else
          LoadTextFile( vList, StringListRestrictedURLs);
      end;
    end;
  end;

  // If no URLs are listed as restrictions then the restriction mode is set
  // to 2, meaning no restriction.
  //if StringListRestrictedURLs.Count < 1 then
  //  fWebBrowserRestrictionMode := 2;


  fEnableFTP := NS_ReadBool('Web Browser - Allow FTP');
  fAccessibleDrives := NS_ReadString('Accessible Drives');
  fTempDirectoryVirtualDrive := Trim(LowerCase(NS_ReadString('TempFolderVirtualDrive')));


  {$ELSE}
  with TRegistry.Create(KEY_READ) do
  try
    RootKey := MyRootKey;
    if OpenKey(NETSTOP_REG_KEY, FALSE) then
    begin
      ButtonClose.Visible := ReadBool('Application Web Browser Displayed - Close');
      ButtonZoom.Visible := ReadBool('Application Web Browser Displayed - Zoom');
      ButtonSearch.Visible := ReadBool('Application Web Browser Displayed - Search') and ComboBoxLocation.Visible;
      ButtonGoHome.Visible := ReadBool('Application Web Browser Displayed - Go Home') and (not fCurrentDCEWebBrowserNavigationRestricted);
      ButtonNewWindow.Visible := ReadBool('Application Web Browser Displayed - New Window') and (not fCurrentDCEWebBrowserNavigationRestricted) and (not IsfFreeClick);
      ButtonBack.Visible := ReadBool('Application Web Browser Displayed - Back') {and (not FormMainMenu.GetCurrentDCEWebBrowserNavigationRestricted) and (not FormMainMenu.IsfFreeClick)};
      ButtonForward.Visible := ReadBool('Application Web Browser Displayed - Forward') {and (not FormMainMenu.GetCurrentDCEWebBrowserNavigationRestricted) and (not FormMainMenu.IsfFreeClick)};
      ButtonOpen.Visible :=  ReadBool('Application Web Browser Displayed - Open');
      ButtonRefresh.Visible := ReadBool('Application Web Browser Displayed - Refresh');
      ButtonStop.Visible := ReadBool('Application Web Browser Displayed - Stop');

      ButtonPrint.Visible := ReadBool('Application Web Browser Displayed - Print') and
                             MyVersionConfig.PrinterAvailable and
                             ((fUsageType in [ord(utAccessCode), ord(utFree), ord(utFreeApplication), ord(utDatabaseUnavailable), ord(utPayMinutesPagesUsed), ord(utCyberCafe), ord(utFreeThenCharge)])
                              or fWebBrowserFree);

      // Setup the download animation
      if FileExists(ExtractFilePath(Application.ExeName) + 'Interface\Animations\Globe30.png') then
        ImageAnimation.Picture.LoadFromFile(ExtractFilePath(Application.ExeName) + 'Interface\Animations\Globe30png');

      BevelAnimation.Top := 5;
      PanelAnimation.Top := BevelAnimation.Top + ((BevelAnimation.Height - ImageAnimation.Height) div 2);
      PanelAnimation.Height := ImageAnimation.Height;
      PanelAnimation.Width := ImageAnimation.Width div 12;
      ImageAnimation.Top := 0;
      ImageAnimation.Left := 0;

      vImageDirectory := ExtractFilePath(Application.ExeName) + 'Interface\Applications\';

      if FileExists(vImageDirectory + 'Close.png') then
        ButtonClose.Glyph.LoadFromFile(vImageDirectory + 'Close.png');
      if FileExists(vImageDirectory + 'DCEWebBrowserBack.png') then
        ButtonBack.Glyph.LoadFromFile(vImageDirectory + 'DCEWebBrowserBack.png');
      if FileExists(vImageDirectory + 'DCEWebBrowserForward.png') then
        ButtonForward.Glyph.LoadFromFile(vImageDirectory + 'DCEWebBrowserForward.png');
      if FileExists(vImageDirectory + 'DCEWebBrowserStop.png') then
        ButtonStop.Glyph.LoadFromFile(vImageDirectory + 'DCEWebBrowserStop.png');
      if FileExists(vImageDirectory + 'DCEWebBrowserRefresh.png') then
        ButtonRefresh.Glyph.LoadFromFile(vImageDirectory + 'DCEWebBrowserRefresh.png');
      if FileExists(vImageDirectory + 'DCEWebBrowserZoom.png') then
        ButtonZoom.Glyph.LoadFromFile(vImageDirectory + 'DCEWebBrowserZoom.png');
      if FileExists(vImageDirectory + 'Print.png') then
        ButtonPrint.Glyph.LoadFromFile(vImageDirectory + 'Print.png');
      if FileExists(vImageDirectory + 'DCEWebBrowserSearch.png') then
        ButtonSearch.Glyph.LoadFromFile(vImageDirectory + 'DCEWebBrowserSearch.png');
      if FileExists(vImageDirectory + 'DCEWebBrowserGoHome.png') then
        ButtonGoHome.Glyph.LoadFromFile(vImageDirectory + 'DCEWebBrowserGoHome.png');
      if FileExists(vImageDirectory + 'DCEWebBrowserOpen.png') then
        ButtonOpen.Glyph.LoadFromFile(vImageDirectory + 'DCEWebBrowserOpen.png');
      if FileExists(vImageDirectory + 'DCEWebBrowserNew.png') then
        ButtonNewWindow.Glyph.LoadFromFile(vImageDirectory + 'DCEWebBrowserNew.png');

      Color := ReadInteger('Application Background - Color');

      if ReadBool('Application Web Browser Button Bar - Displayed') then
      begin
        PanelButtons.Visible := True;

        for vIndex := 1 to 11 do
        begin
          case vIndex of
            1: vButton := ButtonClose;
            2: vButton := ButtonBack;
            3: vButton := ButtonForward;
            4: vButton := ButtonStop;
            5: vButton := ButtonRefresh;
            6: vButton := ButtonZoom;
            7: vButton := ButtonSearch;
            8: vButton := ButtonNewWindow;
            9: vButton := ButtonPrint;
            10: vButton := ButtonOpen;
            11: vButton := ButtonGoHome;
          end;

          with vButton do
          begin
            Height := cButtonsHeight;
            BackColor := ReadInteger('Application Background - Color');
            BorderSize := ReadInteger('Application Buttons - Border Size');

            CornerRound := ReadInteger('Application Buttons - Rounded Value');
            Corners.TopLeft := ReadBool('Application Buttons - Rounded Top Left');
            Corners.TopRight  := ReadBool('Application Buttons - Rounded Top Right');
            Corners.BottomLeft := ReadBool('Application Buttons - Rounded Bottom Left');
            Corners.BottomRight := ReadBool('Application Buttons - Rounded Bottom Right');
            GlyphAutoGray := ReadBool('Application Buttons - Gray Icons');

            Font.Name := ReadString('Application Buttons - Font Name');
            Font.Color := ReadInteger('Application Buttons - Font Color');
            Font.Size := ReadInteger('Application Buttons - Font Size');
            Font.Style := [];
            if ReadBool('Application Buttons - Bold') then
              Font.Style := [fsBold];
            if ReadBool('Application Buttons - Italic') then
              Font.Style := Font.Style + [fsItalic];
            if ReadBool('Application Buttons - Underline') then
              Font.Style := Font.Style + [fsUnderline];

            Properties.Disabled.Color := ReadInteger('Application Buttons - Disabled Color');
            Properties.Disabled.Border := ReadInteger('Application Buttons - Disabled Color Border');
            Properties.Disabled.Font.Name := ReadString('Application Buttons - Font Name');
            Properties.Disabled.Font.Color := ReadInteger('Application Buttons - Disabled Color Font');
            Properties.Disabled.Font.Size := ReadInteger('Application Buttons - Font Size');
            Properties.Disabled.Font.Style := [];
            if ReadBool('Application Buttons - Bold') then
              Properties.Disabled.Font.Style := [fsBold];
            if ReadBool('Application Buttons - Italic') then
              Properties.Disabled.Font.Style := Properties.Disabled.Font.Style + [fsItalic];
            if ReadBool('Application Buttons - Underline') then
              Properties.Disabled.Font.Style := Properties.Disabled.Font.Style + [fsUnderline];

            Properties.MouseAway.Color := ReadInteger('Application Buttons - Color');
            Properties.MouseAway.Border := ReadInteger('Application Buttons - Border Color');
            Properties.MouseAway.Font.Name := ReadString('Application Buttons - Font Name');
            Properties.MouseAway.Font.Color := ReadInteger('Application Buttons - Font Color');
            Properties.MouseAway.Font.Size := ReadInteger('Application Buttons - Font Size');
            Properties.MouseAway.Font.Style := [];
            if ReadBool('Application Buttons - Bold') then
              Properties.MouseAway.Font.Style := [fsBold];
            if ReadBool('Application Buttons - Italic') then
              Properties.MouseAway.Font.Style := Properties.MouseAway.Font.Style + [fsItalic];
            if ReadBool('Application Buttons - Underline') then
              Properties.MouseAway.Font.Style := Properties.MouseAway.Font.Style + [fsUnderline];

            Properties.MouseDown.Color := ReadInteger('Application Buttons - Mouse Down Color');
            Properties.MouseDown.Border := ReadInteger('Application Buttons - Mouse Down Color Border');
            Properties.MouseDown.Font.Name := ReadString('Application Buttons - Font Name');
            Properties.MouseDown.Font.Color := ReadInteger('Application Buttons - Mouse Down Color Font');
            Properties.MouseDown.Font.Size := ReadInteger('Application Buttons - Font Size');
            Properties.MouseDown.Font.Style := [];
            if ReadBool('Application Buttons - Bold') then
              Properties.MouseDown.Font.Style := [fsBold];
            if ReadBool('Application Buttons - Italic') then
              Properties.MouseDown.Font.Style := Properties.MouseDown.Font.Style + [fsItalic];
            if ReadBool('Application Buttons - Underline') then
              Properties.MouseDown.Font.Style := Properties.MouseDown.Font.Style + [fsUnderline];

            Properties.MouseOver.Color := ReadInteger('Application Buttons - Mouse Over Color');
            Properties.MouseOver.Border := ReadInteger('Application Buttons - Mouse Over Color Border');
            Properties.MouseOver.Font.Name := ReadString('Application Buttons - Font Name');
            Properties.MouseOver.Font.Color := ReadInteger('Application Buttons - Mouse Over Color Font');
            Properties.MouseOver.Font.Size := ReadInteger('Application Buttons - Font Size');
            Properties.MouseOver.Font.Style := [];
            if ReadBool('Application Buttons - Bold') then
              Properties.MouseOver.Font.Style := [fsBold];
            if ReadBool('Application Buttons - Italic') then
              Properties.MouseOver.Font.Style := Properties.MouseOver.Font.Style + [fsItalic];
            if ReadBool('Application Buttons - Underline') then
              Properties.MouseOver.Font.Style := Properties.MouseOver.Font.Style + [fsUnderline];

          end;
        end;
      end;

      // Banner
      PanelHTMLBanner.Visible := ReadBool('HTML Banner (Web) - Displayed');
      fHTMLBannerWebHeight := ReadInteger('HTML Banner (Web) - Height');
      PanelHTMLBanner.Height := 0;
      fHTMLBannerWebAddress[0] := ReadString('HTML Banner (Web) - Source Prefix English') + ReadString('HTML Banner (Web) - Source English');
      fHTMLBannerWebAddress[1] := ReadString('HTML Banner (Web) - Source Prefix French') + ReadString('HTML Banner (Web) - Source French');
      fHTMLBannerWebAddress[2] := ReadString('HTML Banner (Web) - Source Prefix Spanish') + ReadString('HTML Banner (Web) - Source Spanish');
      fHTMLBannerWebAddress[3] := ReadString('HTML Banner (Web) - Source Prefix German') + ReadString('HTML Banner (Web) - Source German');
      fHTMLBannerWebAddress[4] := ReadString('HTML Banner (Web) - Source Prefix Portuguese') + ReadString('HTML Banner (Web) - Source Portuguese');
      fHTMLBannerWebAddress[5] := ReadString('HTML Banner (Web) - Source Prefix Italian') + ReadString('HTML Banner (Web) - Source Italian');
      fHTMLBannerWebAddress[6] := ReadString('HTML Banner (Web) - Source Prefix Other 1') + ReadString('HTML Banner (Web) - Source Other 1');
      fHTMLBannerWebAddress[7] := ReadString('HTML Banner (Web) - Source Prefix Other 2') + ReadString('HTML Banner (Web) - Source Other 2');
      fHTMLBannerWebEnglishAlwaysDisplayed := ReadBool('HTML Banner (Web) - English Always Displayed');
      fHTMLBannerWebReloadSeconds := ReadInteger('HTML Banner (Web) - Reload Seconds') * 1000;

      // Text
      Font.Name := ReadString('Application Text - Font Name');
      Font.Color := ReadInteger('Application Text - Font Color');
      Font.Size := ReadInteger('Application Text - Font Size');
      Font.Style := [];
      if ReadBool('Application Text - Bold') then
        Font.Style := [fsBold];
      if ReadBool('Application Text - Italic') then
        Font.Style := Font.Style + [fsItalic];
      if ReadBool('Application Text - Underline') then
        Font.Style := Font.Style + [fsUnderline];

      fStatusBarHeight := ReadInteger('Status Bar - Height');
      fKioskDefaultLanguage := ReadInteger('Languages - Default Language');
      // Web Browser
      fWebBrowserStartPage[0] := ReadString('Web Browser - Start Page English Prefix') +
                                 ReadString('Web Browser - Start Page English');
      fWebBrowserStartPage[1] := ReadString('Web Browser - Start Page French Prefix') +
                                 ReadString('Web Browser - Start Page French');
      fWebBrowserStartPage[2] := ReadString('Web Browser - Start Page Spanish Prefix') +
                                 ReadString('Web Browser - Start Page Spanish');
      fWebBrowserStartPage[3] := ReadString('Web Browser - Start Page German Prefix') +
                                 ReadString('Web Browser - Start Page German');
      fWebBrowserStartPage[4] := ReadString('Web Browser - Start Page Portuguese Prefix') +
                                 ReadString('Web Browser - Start Page Portuguese');
      fWebBrowserStartPage[5] := ReadString('Web Browser - Start Page Italian Prefix') +
                                 ReadString('Web Browser - Start Page Italian');
      fWebBrowserStartPage[6] := ReadString('Web Browser - Start Page OtherLanguage1 Prefix') +
                                 ReadString('Web Browser - Start Page OtherLanguage1');
      fWebBrowserStartPage[7] := ReadString('Web Browser - Start Page OtherLanguage2 Prefix') +
                                 ReadString('Web Browser - Start Page OtherLanguage2');
      fWebBrowserStartPage[8] := '';
      fWebBrowserStartPage[9] := '';
      fWebBrowserStartPage[10] := ReadString('Web Browser - Start Page Chinese Prefix') +
                                  ReadString('Web Browser - Start Page Chinese');


      fWebBrowserSearchPage[0] := ReadString('Web Browser - Search Page English Prefix') +
                                  ReadString('Web Browser - Search Page English');
      fWebBrowserSearchPage[1] := ReadString('Web Browser - Search Page French Prefix') +
                                  ReadString('Web Browser - Search Page French');
      fWebBrowserSearchPage[2] := ReadString('Web Browser - Search Page Spanish Prefix') +
                                  ReadString('Web Browser - Search Page Spanish');
      fWebBrowserSearchPage[3] := ReadString('Web Browser - Search Page German Prefix') +
                                  ReadString('Web Browser - Search Page German');
      fWebBrowserSearchPage[4] := ReadString('Web Browser - Search Page Portuguese Prefix') +
                                  ReadString('Web Browser - Search Page Portuguese');
      fWebBrowserSearchPage[5] := ReadString('Web Browser - Search Page Italian Prefix') +
                                  ReadString('Web Browser - Search Page Italian');
      fWebBrowserSearchPage[6] := ReadString('Web Browser - Search Page OtherLanguage1 Prefix') +
                                  ReadString('Web Browser - Search Page OtherLanguage1');
      fWebBrowserSearchPage[7] := ReadString('Web Browser - Search Page OtherLanguage2 Prefix') +
                                  ReadString('Web Browser - Search Page OtherLanguage2');
      fWebBrowserSearchPage[8] := '';
      fWebBrowserSearchPage[9] := '';
      fWebBrowserSearchPage[10] := ReadString('Web Browser - Search Page Chinese Prefix') +
                                  ReadString('Web Browser - Search Page Chinese');
      fWebBrowserRightClick := ReadBool('Web Browser - Right Click');

      for vIndex := 1 to 20 do
      begin
        fOtherApplicationOnlyAllowAccessTo[vIndex] := ReadBool('Other Applications ' + IntToStr(vIndex) + ' - Only Allow Access To');
        fOtherApplicationUseURLRestrictionsFromFile[vIndex] := ReadBool('Other Applications ' + IntToStr(vIndex) + ' - Load URL Restrictions From File');
      end;

      fWebBrowserAllowTypedURLs := ReadBool('Web Browser - Allow Typed URLs');
      fWebBrowserFreeButAllowTypedURLs := ReadBool('Web Browser - Free But Allow Typed URLs');
      fWebBrowserAllowLocalDriveAccess := ReadBool('Web Browser - Allow Local Drive Access');

      vList := ReadString('Web Browser - Allow Printing URLs');
      while Pos('&', vList) <> 0 do
      begin
        StringListAllowToPrintURLs.Add(copy(vList, 1, Pos('&', vList) - 1));
        vList := LowerCase(copy(vList, Pos('&', vList) + 1, Length(vList) - Pos('&', vList)));
      end;

      vList := ReadString('Web Browser');
      while Pos('&', vList) <> 0 do
      begin
        StringListRestrictedURLs.Add(copy(vList, 1, Pos('&', vList) - 1));
        vList := LowerCase(copy(vList, Pos('&', vList) + 1, Length(vList) - Pos('&', vList)));
      end;

      fEnableFTP := ReadBool('Web Browser - Allow FTP');
      fAccessibleDrives := ReadString('Accessible Drives');
      fTempDirectoryVirtualDrive Trim(LowerCase(ReadString('TempFolderVirtualDrive')));

      CloseKey;
    end;
  finally
    free;
  end;
  {$ENDIF}

  //TimerHTMLBannerWeb.Enabled := PanelHTMLBanner.Visible;
  //TimerCEFBrowserWindow.Enabled := True;
end;


procedure TFormDCEWebBrowser.rkSmartTabsAddClick(Sender: TObject);
var
  vlTS: TTabSheet;
  vlWBFrame: TWBFrame;
begin
  vlTS := TTabSheet.Create(PgCtrl);
  vlTS.Caption := ComboBoxLocation.Text;
  rkSmartTabs.AddTab(ComboBoxLocation.Text);
  vlTS.PageControl := PgCtrl;
  PgCtrl.SelectNextPage(True,True);
  vlWBFrame := TWBFrame.Create(vlTS);
  vlWBFrame.Parent := vlTS;
  vlWBFrame.Align := alClient;
  vlWBFrame.Chrm.Load(ComboBoxLocation.Text);
end;


procedure TFormDCEWebBrowser.rkSmartTabsClick(Sender: TObject);
begin
  PgCtrl.ActivePageIndex := rkSmartTabs.ActiveTab;
end;

procedure TFormDCEWebBrowser.rkSmartTabsCloseTab(Sender: TObject;
  Index: Integer; var Close: Boolean);
var
  i : integer;
begin
  i := rkSmartTabs.ActiveTab;
  if i > 0 then begin
    PgCtrl.Pages[i].Destroy;
    Close := True;
  end else
    Close := False;
end;

{
procedure TFormDCEWebBrowser.rkSmartTabsCloseTab(Sender: TObject;
  Index: Integer; var Close: Boolean);
var
  f,p : integer;
begin
  CurrentTab := Index;

  if (Index = 0) and (rkSmartTabs.Tabs.Count = 1) then
  begin
    Close := False;
    ButtonCLoseClick(nil);
    Exit;
  end;

  //mh 6/14/2015
  FreeAndNil(aWBFrame[Index]);
  FreeAndNil(aCRMPanel[Index]);
  f := Index + 1;
  Move(aWBFrame[f], aWBFrame[(f - 1)], SizeOf(aWBFrame) * (Length(aWBFrame) - (f - 1) - 1));
  Move(aCRMPanel[f], aCRMPanel[(f - 1)], SizeOf(aCRMPanel) * (Length(aCRMPanel) - (f - 1) - 1));

  //FreeAndNil(aCRMPanel[Index]);
  //for p := (Index + 1) to High(aCRMPanel) do
  // begin
  //   Move(aCRMPanel[p], aCRMPanel[(p - 1)], SizeOf(aCRMPanel) * (Length(aCRMPanel) - (p - 1) - 1));
  //end;


  Close := True;
end;
}

procedure TFormDCEWebBrowser.rkSmartTabsGetImageIndex(Sender: TObject;
  Tab: Integer; var Index: Integer);
begin
  Index := 0;
end;

{
procedure TFormDCEWebBrowser.rkSmartTabsTabChange(Sender: TObject);
var
  i,n : integer;
begin
  i := rkSmartTabs.ActiveTab;

  for n := 0 to High(aWBFrame) do
  begin
    if aWBFrame[n] <> nil then
      aCrmPanel[n].Visible := False;
  end;

  aCrmPanel[i].Visible := True;

  ComboBoxLocation.ItemIndex := ComboboxLocation.Items.IndexOf(aWBFrame[i].Chrm.Browser.MainFrame.Url);
end;
}

procedure TFormDCEWebBrowser.UpdateToolBarDimensions;
var
  vControl: TControl;
  vIndex: Integer;
begin
  vControl := nil;
  fToolBarWidth := cToolBarSpacing;

  for vIndex := 1 to 11 do
  begin
    case vIndex of
      1: vControl := ButtonClose;
      2: vControl := ButtonBack;
      3: vControl := ButtonForward;
      4: vControl := ButtonStop;
      5: vControl := ButtonRefresh;
      6: vControl := ButtonZoom;
      7: vControl := ButtonSearch;
      8: vControl := ButtonPrint;
      9: vControl := ButtonNewWindow;
      10: vControl := ButtonGoHome;
      11: vControl := ButtonOpen;
    end;

    if (vControl.Visible) then
    begin
      //mh 5/13/2009
      Canvas.Lock;

      Canvas.Font := (vControl as TIBEAntialiasButton).Font;
      vControl.Width := (vControl as TIBEAntialiasButton).Glyph.Width +
                        (vControl as TIBEAntialiasButton).GlyphSeparation +
                        (Canvas.TextWidth((vControl as TIBEAntialiasButton).Caption )) +
                        (vControl as TIBEAntialiasButton).Margins.Left +
                        (vControl as TIBEAntialiasButton).Margins.Right +
                        ((vControl as TIBEAntialiasButton).BorderSize * 2);
      fToolBarWidth := fToolBarWidth + vControl.Width + cToolBarSpacing;
      //mh 5/13/2009
      Canvas.Unlock;
    end;
  end;
end;

procedure TFormDCEWebBrowser.UpdateToolBarPositions;
var
  vIndex: Integer;
  vCount: Integer;
  vControl: TControl;
  vTop: Integer;
  vTempPanelButtons: Integer;
  vTotalBarWidthAlreadyExtended: Boolean;
  vTotalBarWidth: Integer; // Contains actual width reserved for buttons on toolbar
begin
  vControl := nil;
  //mh
  ShellProLog.DLog('UpdateToolBarPositions');

  //vTotalBarWidth := FormMainMenu.GetAvailableScreenWidth - (BevelAnimation.Width + cToolBarSpacing);
  vTotalBarWidth := Screen.Width - (BevelAnimation.Width + cToolBarSpacing);
  vTempPanelButtons := 52; // Setting default height
  ComboBoxLocation.Width := 225; //Setting minimum width

  // When populating the second row of buttons, we have more room because there is no animation.
  // This flag is too make sure with don't add it again on the 3rd row...
  vTotalBarWidthAlreadyExtended := False;

  // Set Location
  vTop := 5;
  vCount := cToolBarSpacing;
  for vIndex := 1 to 12 do
  begin
    case vIndex of
      1: vControl := ButtonClose;
      2: vControl := ButtonBack;
      3: vControl := ButtonForward;
      4: vControl := ButtonStop;
      5: vControl := ButtonRefresh;
      6: vControl := ButtonZoom;
      7: vControl := ButtonSearch;
      8: vControl := ButtonPrint;
      9: vControl := ButtonNewWindow;
      10: vControl := ButtonGoHome;
      11: vControl := ComboBoxLocation;
      12: vControl := ButtonOpen;
    end;

    if (vControl.Visible) then
    begin
      if ((vCount + vControl.Width + cToolBarSpacing) > vTotalBarWidth) then
      begin
        // Right align with animation
        if not vTotalBarWidthAlreadyExtended then
          vTotalBarWidth := vTotalBarWidth + cButtonsHeight + cToolBarSpacing;
        vTotalBarWidthAlreadyExtended := True;

        vTempPanelButtons := vTempPanelButtons + cButtonsHeight + 5;
        vTop := vTop + cButtonsHeight + 5;
        vCount := cToolBarSpacing;
      end;

      vControl.Top := vTop;

      if (vIndex = 11) then
      begin
        vControl.Top := vTop + ((cButtonsHeight - ComboBoxLocation.Height) div 2);
        try
          if ButtonOpen.Visible then
            ComboBoxLocation.Width := vTotalBarWidth - (vCount + ButtonOpen.Width + 10)
          else
            ComboBoxLocation.Width := vTotalBarWidth - (vCount + 10);
        except
         ShellProLog.DLog('ERR - TFormDCEWebBrowser.UpdateToolBarPositions - ComboBoxLocation Error');
        end;

      end;

      vControl.Left := vCount;
      vCount := vCount + vControl.Width + cToolBarSpacing;
    end;
  end;

  // Prevents flickering
  if (PanelButtons.Height <> vTempPanelButtons) then
    PanelButtons.Height := vTempPanelButtons;
end;

procedure TFormDCEWebBrowser.SetStatusBar;
begin
  StatusBarWebBrowser.Panels[0].Width := StatusBarWebBrowser.Width - 234;
  StatusBarWebBrowser.Panels[1].Width := 40;
  StatusBarWebBrowser.Panels[2].Width := 194;

  //ImageLock.Left := StatusBarWebBrowser.Width - 224;

  Progressbar.Height := StatusbarWebBrowser.Height - 8;
  Progressbar.Top := 4;
  Progressbar.Left := StatusBarWebBrowser.Width - 200;
  Progressbar.Width := 190;

end;

procedure TFormDCEWebBrowser.SetZooomLevel(const Value: integer);
begin
  FZooomLevel := Value;
end;

procedure TFormDCEWebBrowser.WMActivate(var Msg: TWMActivate);
var
  S: WideString;
  wnd: HWND;
  I: Integer;
begin
  {This procedure is used to detect some IE Screens}
  If Msg.Active=0 then
  begin
    wnd := Msg.ActiveWindow;
    I := GetWindowTextLength(wnd);
    SetLength(S, I + 1);
    GetWindowText(Wnd, PWideChar(S), I + 1);

    //Close Print Screen
    //mh 1/27/2011

    if (Pos('print', LowerCase(S))>0) and
       (((fWebBrowserRestrictPrintingMode = 0) and (fAllowWebPageToPrint)) or
        ((fWebBrowserRestrictPrintingMode = 1) and (not fAllowWebPageToPrint))) then
    begin
      PostMessage(wnd,WM_CLOSE,0,0);
      IPCSynchFromShellPro('SetDisplayNotAllowPrintingMessageTrue');
      //FormMainMenu.SetDisplayNotAllowPrintingMessage(True);
    end;

  end;
end;

procedure TFormDCEWebBrowser.DCEWebBrowserScriptError(Sender: TObject; ErrorLine,
  ErrorCharacter, ErrorCode, ErrorMessage, ErrorUrl: String;
  var ContinueScript, Showdialog: Boolean);
begin
    ShowDialog     := FALSE;
    ContinueScript := TRUE;
end;


procedure TFormDCEWebBrowser.PrintButtonTimerTimer(Sender: TObject);
var
  hHwnd : Hwnd;
begin
  ShellProLog.DLog('DCEWebBrowser.PrintButtonTimer - Enter');
  hHwnd := FindWindow(pWideChar('#32770'), pWideChar('Print'));
  if hHwnd <> 0 then Exit;

  Sleep(5000);

  if PrintButtonTimerEnabled then
  begin
    ShellProLog.DLog('DCEWebBrowser.PrintButtonTimer - Enabling Print Button');
    fPrintingEnabled := True;
    ButtonPrint.Enabled := True;
    PrintButtonTimer.Enabled := False;
  end;
end;


procedure TFormDCEWebBrowser.DCEWebBrowserWindowSetLeft(Sender: TObject;
  Left: Integer);
begin
  (Sender as TFormDCEWebBrowser).Left := Left;
end;

procedure TFormDCEWebBrowser.DCEWebBrowserWindowSetWidth(Sender: TObject;
  Width: Integer);
begin
  (Sender as TFormDCEWebBrowser).Width := Width;
end;


function TFormDCEWebBrowser.DisplayFNMessageForm(pMessageDisplayed: String;
  pButtonsDisplayed: Integer; pImageDisplayed: String;
  pDuration: Integer): Integer;
//var
//  TFF : TFormMessage;
begin
  Result := 0;
  {
  try
    //mh 4/25/2013 changed from application to nil.  Should be freed when finished with modal
    TFF := TFormMessage.Create(nil, fKioskLanguage);
  except
    on E:Exception do
    begin
      //mh 4/25/2013
      if assigned(TFF) then
        TFF.Free;

      ShellProLog.CreateLogEntry('ERR-026A', 'Unable to create TFormMessage. Exception Occured: ' + E.Message, TRUE);  // old code: Showmessage('ERR-026A');
      EXIT;
    end;
  end;
  }
  with TFormMessage.Create(nil, fKioskLanguage) do
  //with TFF do
  begin
    try
      try
//ShellProLog.DLog('1');
        // Set visibility of buttons.
        ButtonYes.Visible := (pButtonsDisplayed = 1) or (pButtonsDisplayed = 2) or (pButtonsDisplayed = 5) or (pButtonsDisplayed = 7) or (pButtonsDisplayed = 8) or (pButtonsDisplayed = 9) or (pButtonsDisplayed = 98); // pButtonsDisplayed=9 added in ver 5.0 issue 106
        ButtonNo.Visible := (pButtonsDisplayed = 1) or (pButtonsDisplayed = 2) or (pButtonsDisplayed = 5) or (pButtonsDisplayed = 7) or (pButtonsDisplayed = 8) or (pButtonsDisplayed = 9) or (pButtonsDisplayed = 98); // pButtonsDisplayed=9 new ver 5.0 isue 160
        ButtonCancel.Visible := (pButtonsDisplayed = 2) or (pButtonsDisplayed = 6) or (pButtonsDisplayed = 9); // pButtonsDisplayed=9 new ver 5.0 isue 160
        ButtonOk.Visible := (pButtonsDisplayed = 0) or (pButtonsDisplayed = 4);

        //Set the alignment
        LabelMessage.Alignment := TAlignment(0);

        // Set the width of the text to 221.
        LabelMessage.Caption := '';
        LabelMessage.Width := 230;

        // Set message to be displayed.
        LabelMessage.Caption := pMessageDisplayed;
//ShellProLog.DLog('2');
        // Set the form smaller if none of the buttons are displayed.
        if (pButtonsDisplayed = 3) then
        begin
          // The window must be at least 90 height so that the image can be
          // displayed properly.
          Height := Max(LabelMessage.Height + 48, 92);
        end
        else
        begin
          Height := Max(LabelMessage.Height + 90, 122);
        end;
//ShellProLog.DLog('3');
        if (pButtonsDisplayed = 3) or (pButtonsDisplayed = 4) or (pButtonsDisplayed = 5)  or (pButtonsDisplayed = 6) or (pButtonsDisplayed = 8) or (pButtonsDisplayed = 98) then
        begin
          TimerClose.Interval := pDuration;
          TimerClose.Enabled := True;
        end
        else
          TimerClose.Enabled := False;

        try
          if FileExists(ExtractFilePath(Forms.Application.ExeName) +  'Interface\Applications\' + pImageDisplayed + '.png') then
            ImageInfoSign.Picture.LoadFromFile(ExtractFilePath(Forms.Application.ExeName) +  'Interface\Applications\' + pImageDisplayed + '.png');
        except
        end;
 //ShellProLog.DLog('4');
        // Assign default button to ButtonNo  - Lenis
        if (pButtonsDisplayed = 7) or (pButtonsDisplayed = 8) or (pButtonsDisplayed = 98) then
          fDefaultButtonNo := True
        else
          fDefaultButtonNo := False;

        // BEGIN ACME 106
        if (pButtonsDisplayed = 9) then begin // Button Configuration for download prompt
          ButtonYes.Left := 28;
          ButtonNo.Left := 119;
          ButtonCancel.Left := 209;
          ButtonYes.Caption := GetTextFromResourceOrRegistry(fKioskLanguage, 258);
          ButtonNo.Caption :=  GetTextFromResourceOrRegistry(fKioskLanguage, 257);
        end;
        // END ACME 106
 //ShellProLog.DLog('5');
        //mh 9/16/2010
        // Move the Yes and No buttons to the left if the cancel button is not
        // displayed.
        if (pButtonsDisplayed = 1) or (pButtonsDisplayed = 5) or (pButtonsDisplayed = 98) then
        begin
          //ButtonYes.Left := 107;
          ButtonYes.Left := Width - ButtonYes.Width - 8 - ButtonNo.Width - 16;
          //ButtonNo.Left := 203;
          ButtonNo.Left := Width - ButtonNo.Width - 16;
        end;

        //mh 11/12/2010
        ShellProLog.DLog('DisplayMessageForm Message: ' + pMessageDisplayed);

        //BringToTop;
        ShowModal;
        Result := ModalResult;

      except
        on E: Exception do
          begin
            ShellProLog.CreateLogEntry('ERR-026B:',e.Message, TRUE);  //     ShowMessage('ERR-026B');      Free;
          end;
      end;
    finally

      Free;
    end;
  end;//with
end;


procedure TFormDCEWebBrowser.DCEWebBrowserWindowSetHeight(Sender: TObject;
  Height: Integer);
begin
  (Sender as TFormDCEWebBrowser).Height := Height;
end;

procedure TFormDCEWebBrowser.DCEWebBrowserWindowSetTop(Sender: TObject;
  Top: Integer);
begin
  (Sender as TFormDCEWebBrowser).Top := Top;
end;

procedure TFormDCEWebBrowser.SetsCurrentURL(const Value: string);
begin
  FsCurrentURL := Value;
end;

procedure TFormDCEWebBrowser.SetsCurrentFileSpec(const Value: String);
begin
  FsCurrentFileSpec := Value;
end;


procedure TFormDCEWebBrowser.TimerFileDialogTimer(Sender: TObject);
//var
 //fd : TFormFileManager;
begin
  if (FileExists(String(sCurrentFileSpec))) or
     (DirectoryExists(String(sCurrentFileSpec))) then
  begin
    TimerFileDialog.Enabled := False;

    //trigger call to FileManager from within ShellPro -
    //just calling filemanager from here is nfg.
    IPCSynchFromShellPro('CallFileManager');

  end;
end;

//MH 9/21/2009 - for CSB to try to keep Winword 2007 from becoming minimized if multiple
//instances open
//MH 12/18/2009 - Original thought did not work.  Now trying battering ram approach.
// if downloading a .doc file, trigger alt-tab to try to force word to front.
// Impossible to detect flashing icon in taskbar - this appears to be the only way.
// Sometimes causes multiple alt-tabs
procedure TFormDCEWebBrowser.TimerOpusAppTimer(Sender: TObject);
begin
  Sleep(100);
  Self.SetFocus;

  TimerOpusApp.Enabled := False;

  keybd_event(VK_MENU, 0, 0, 0);
  Sleep(10);
  keybd_event(VK_TAB, 0, 0, 0);
  Sleep(100);
  // stop pressing "Alt-Tab"
  keybd_event(VK_MENU, 0, KEYEVENTF_KEYUP, 0);
  Sleep(10);
  keybd_event(VK_TAB, 0, KEYEVENTF_KEYUP, 0);
  Sleep(500);
end;


procedure TFormDCEWebBrowser.SetIsClosing(const Value: boolean);
begin FIsClosing := Value; end;

Initialization

  // This is supposed to help prevent access violations with the web browser.
  OleInitialize(nil);
  Set8087cw($133f);
  //only use this to persist cookies and cached items between runs.
  //CefCache := 'cache';

  CefSingleProcess := False;
  if not CefLoadLibDefault then
    Exit;

finalization
  // This is supposed to prevent error with TWebBrowser.
  OleUninitialize;

end.



