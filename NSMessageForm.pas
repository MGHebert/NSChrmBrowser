//
//              (C) Moonrise Systems Inc 1999
//              ALL RIGHTS RESERVED.
//__________________________________________________________________________
// Unit File  : NSMessageForm.PAS
// Author     : Yves Mailhot
//__________________________________________________________________________
//
// Description
//
//   This unit is the implementation of a dialog used to ask the user
//   a question or to display a message for which a response may
//   or may not be expected before proceding.
//__________________________________________________________________________
//
// 07/18/2015 - Copied from MessageForm.pas from ShellPro. Idea is to isolate
//              for use by NSChrBrowser without using Main units form ShellPro
//
//

unit NSMessageForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, YMColorButton, IBEAntialiasButton,  IBEButton, Superbvlplus,
  TntStdCtrls, LogFile, OleCtrls, SHDocVw, NSDatSettingsUtils, cefvcl;

type
  TFormMessage = class(TForm)
    TimerClose: TTimer;

    // The following TEdit is used only because forms without any focus
    // sometimes won't disappear from the screen. A TEdit 0 x 0 is used to make
    // sure at least one component can have the focus.
    EditDummy: TEdit;
    PanelMain: TPanel;
    ButtonOk: TIBEAntialiasButton;
    ButtonYes: TIBEAntialiasButton;
    ButtonNo: TIBEAntialiasButton;
    ButtonCancel: TIBEAntialiasButton;
    ImageInfoSign: TImage;
    LabelMessage: TLabel;
    BevelMain: TSuperBevelPlus;
    PanelWB: TPanel;
    wbr: TChromium;
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure ButtonYesClick(Sender: TObject);
    procedure ButtonNoClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonOkClick(Sender: TObject);
    procedure TimerCloseTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    FLeft: Integer;
    FTop: Integer;
    FCanMove: Boolean;
    FLanguage: Integer;
    procedure SetLanguage;
    procedure ReadConfiguration;
  protected
    procedure WMPosChange(var Message: TWMWindowPosChanging);  message WM_WINDOWPOSCHANGING;
  public
    // Public declarations
    FDefaultButtonNo : Boolean;
    constructor Create(AOwner: TComponent; ALanguage: Integer); reintroduce;
  end;

implementation

uses Utilities, Constants, Registry, OtherLanguages;

{$R *.DFM}
{===============================================================================

===============================================================================}

function DisplayMessageForm(AMessageDisplayed: String; AButtonsDisplayed: Integer; AImageDisplayed: String; ADuration: Integer; ALanguage: Integer): Integer;
var
  TF : TFormMessage;
begin
  Result := 0;

  try
    TF := TFormMessage.Create(nil, ALanguage);
  except
    on E:Exception do
    begin
      ShellProLog.CreateLogEntry('ERR-026A', 'Unable to create TFormMessage. Exception Occured: ' + E.Message, TRUE);  // old code: Showmessage('ERR-026A');
      EXIT;
    end;
  end;

  //with TFormMessage.Create(Application) do
  with TF do
  begin
    try
      try
        // Set visibility of buttons.
        ButtonYes.Visible := (AButtonsDisplayed = 1) or (AButtonsDisplayed = 2) or (AButtonsDisplayed = 5) or (AButtonsDisplayed = 7) or (AButtonsDisplayed = 8) or (AButtonsDisplayed = 9); // pButtonsDisplayed=9 added in ver 5.0 issue 106
        ButtonNo.Visible := (AButtonsDisplayed = 1) or (AButtonsDisplayed = 2) or (AButtonsDisplayed = 5) or (AButtonsDisplayed = 7) or (AButtonsDisplayed = 8) or (AButtonsDisplayed = 9); // pButtonsDisplayed=9 new ver 5.0 isue 160
        ButtonCancel.Visible := (AButtonsDisplayed = 2) or (AButtonsDisplayed = 6) or (AButtonsDisplayed = 9); // pButtonsDisplayed=9 new ver 5.0 isue 160
        ButtonOk.Visible := (AButtonsDisplayed = 0) or (AButtonsDisplayed = 4);

        // Move the Yes and No buttons to the left if the cancel button is not
        // displayed.
        if (AButtonsDisplayed = 1) or (AButtonsDisplayed = 5) then
        begin
          ButtonYes.Left := 107;
          ButtonNo.Left := 203;
        end;

        //Set the alignment
        LabelMessage.Alignment := TAlignment(0);

        // Set the width of the text to 221.
        LabelMessage.Caption := '';
        LabelMessage.Width := 230;

        // Set message to be displayed.
        LabelMessage.Caption := AMessageDisplayed;

        // Set the form smaller if none of the buttons are displayed.
        if (AButtonsDisplayed = 3) then
        begin
          // The window must be at least 90 height so that the image can be
          // displayed properly.
          Height := Max(LabelMessage.Height + 48, 92);
        end
        else
        begin
          Height := Max(LabelMessage.Height + 90, 122);
        end;

        if (AButtonsDisplayed = 3) or (AButtonsDisplayed = 4) or (AButtonsDisplayed = 5)  or (AButtonsDisplayed = 6) or (AButtonsDisplayed = 8) then
        begin
          TimerClose.Interval := ADuration;
          TimerClose.Enabled := True;
        end
        else
          TimerClose.Enabled := False;

        try
          if FileExists(ExtractFilePath(Application.ExeName) +  'Interface\Applications\' + AImageDisplayed + '.png') then
            ImageInfoSign.Picture.LoadFromFile(ExtractFilePath(Application.ExeName) +  'Interface\Applications\' + AImageDisplayed + '.png');
        except
        end;

        // Assign default button to ButtonNo  - Lenis
        if (AButtonsDisplayed = 7) or (AButtonsDisplayed = 8) then
          FDefaultButtonNo := True
        else
          FDefaultButtonNo := False;

        // BEGIN ACME 106
        if (AButtonsDisplayed = 9) then begin // Button Configuration for download prompt
          ButtonYes.Left := 28;
          ButtonNo.Left := 119;
          ButtonCancel.Left := 209;
          ButtonYes.Caption := GetTextFromResourceOrRegistry(ALanguage, 258);
          ButtonNo.Caption :=  GetTextFromResourceOrRegistry(ALanguage, 257);
        end;
        // END ACME 106

        //if ButtonYes.visible then
        //  ButtonYes.SetFocus;

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


constructor TFormMessage.Create(AOwner: TComponent; ALanguage: Integer);
begin
  inherited Create(AOwner);
  FDefaultButtonNo := False;
  FLanguage := ALanguage;
  SetLanguage;
  ReadConfiguration;
  FCanMove := True;
end;

procedure TFormMessage.WMPosChange(var Message: TWMWindowPosChanging);
begin
  if not FCanMove then
    PWindowPos(TMessage(Message).lParam).Flags := PWindowPos(TMessage(Message).lParam).Flags or SWP_NOMOVE
  else
    inherited;
end;

// This procedure is called when a key is pressed.  If the key is Enter, then
// it is the equivalent to clicking on the Ok button or the Yes button.
// If the key is Esc, then it is the equivalent to clicking on the cancel
// Button or the No button. If both the Cancal button and the No button are
// displayed, then Esc corresponds to Cancel.
procedure TFormMessage.FormKeyPress(Sender: TObject; var Key: Char);
begin
  // Check if the default button is No - Lenis
  if (Key = #13) and (fDefaultButtonNo = True) then
  begin
    if ButtonNo.Visible then
      ButtonNo.Click;

    Key := #0;
    Exit;
  end;

  // Check if the Enter key was pressed.
  if (Key = #13)then
  begin
    if ButtonYes.Visible then
      ButtonYesClick(Self)
    else
      if ButtonOk.Visible then
        ButtonOkClick(Self);
    Key := #0;
  end;

  // Check if the Esc key was pressed.
  if (Key = #27)then
  begin
    if ButtonCancel.Visible then
      ButtonCancelClick(Self)
    else
      if ButtonNo.Visible then
        ButtonNoClick(Self);
    Key := #0;
  end;
end;

// This procedure is called when the Yes button is clicked.
procedure TFormMessage.ButtonYesClick(Sender: TObject);
begin
  ModalResult := mrYes;
end;

// This procedure is called when the No button is clicked.
procedure TFormMessage.ButtonNoClick(Sender: TObject);
begin
  ModalResult := mrNo;
end;

// This procedure is called when the Cancel button is clicked.
procedure TFormMessage.ButtonCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

// This procedure is called when the Ok button is clicked.
procedure TFormMessage.ButtonOkClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

// This procedure is called when the timer reaches the specified number of
// seconds. It is used to automatically close the message form.
procedure TFormMessage.TimerCloseTimer(Sender: TObject);
begin
  ModalResult := mrIgnore;
  Close;
end;

procedure TFormMessage.ReadConfiguration;
var
  vButton: TIBEAntialiasButton;
  vIndex: Integer;
begin
  vButton := nil;
  ShellProLog.DLog('TFormMessage.ReadConfiguration - Enter');

  {$IFDEF DATSETTINGS}
  // Display
  Color := NS_ReadInteger('Application Background - Color');
  // Set properties for all 4 buttons
  for vIndex := 1 to 4 do
  begin
    case vIndex of
      1: vButton := ButtonOk;
      2: vButton := ButtonYes;
      3: vButton := ButtonNo;
      4: vButton := ButtonCancel;
    end;
    with vButton do
    begin
      BackColor := NS_ReadInteger('Application Background - Color');
      CornerRound := NS_ReadInteger('Application Buttons - Rounded Value');
      Corners.TopLeft := NS_ReadBool('Application Buttons - Rounded Top Left');
      Corners.TopRight  := NS_ReadBool('Application Buttons - Rounded Top Right');
      Corners.BottomLeft := NS_ReadBool('Application Buttons - Rounded Bottom Left');
      Corners.BottomRight := NS_ReadBool('Application Buttons - Rounded Bottom Right');
      GlyphAutoGray := NS_ReadBool('Application Buttons - Gray Icons');
      BorderSize := NS_ReadInteger('Application Buttons - Border Size');
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

  {$ELSE}
  with TRegistry.Create(KEY_READ) do
  try
    RootKey := MyRootKey;
    if OpenKey(NETSTOP_REG_KEY, False) then
    begin
      // Display
      Color := ReadInteger('Application Background - Color');

      // Set properties for all 4 buttons
      for vIndex := 1 to 4 do
      begin
        case vIndex of
          1: vButton := ButtonOk;
          2: vButton := ButtonYes;
          3: vButton := ButtonNo;
          4: vButton := ButtonCancel;
        end;

        with vButton do
        begin
          BackColor := ReadInteger('Application Background - Color');
          CornerRound := ReadInteger('Application Buttons - Rounded Value');
          Corners.TopLeft := ReadBool('Application Buttons - Rounded Top Left');
          Corners.TopRight  := ReadBool('Application Buttons - Rounded Top Right');
          Corners.BottomLeft := ReadBool('Application Buttons - Rounded Bottom Left');
          Corners.BottomRight := ReadBool('Application Buttons - Rounded Bottom Right');
          GlyphAutoGray := ReadBool('Application Buttons - Gray Icons');
          BorderSize := ReadInteger('Application Buttons - Border Size');
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


    CloseKey;
  finally
    //free;
  end;
  {$ENDIF}

  ShellProLog.DLog('TFormMessage.ReadConfiguration - Exit');
end;

// This procedure sets the text to the current language as selected by the user.
// Strings of characters are retrieved from resource files.
procedure TFormMessage.SetLanguage;
begin
  ShellProLog.DLog('TFormMessage.SetLanguage - Enter');
  ButtonYes.Caption := GetTextFromResourceOrRegistry(FLanguage, 261);
  ButtonNo.Caption := GetTextFromResourceOrRegistry(FLanguage, 262);
  ButtonCancel.Caption := GetTextFromResourceOrRegistry(FLanguage, 250);
  ButtonOk.Caption := GetTextFromResourceOrRegistry(FLanguage, 260);
  ShellProLog.DLog('TFormMessage.SetLanguage - Exit');
end;

procedure TFormMessage.FormShow(Sender: TObject);
begin
  Fleft := Self.Left;
  FTop := Self.Top;
  // Make the dafault button No
  if FDefaultButtonNo then
  begin
    ButtonYes.Default := False;
    ButtonNo.Default := True;
  end;
  //mh 4/1/2009
  BringToFront;
  //mh 12/3/2010
  SetFocus;
end;

procedure TFormMessage.FormActivate(Sender: TObject);
begin
  FCanMove := False;
end;

end.
