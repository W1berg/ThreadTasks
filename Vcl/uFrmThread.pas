// Copyright (C) 2023 Pontus Wiberg <wiberg.public@outlook.com>
// Licensed under GPLv2 or later version. Contact for any licensing concerns

unit uFrmThread;

interface

uses
  uTypes,

  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.Buttons;

type
  TFrameThread = class(TFrame)
    btnStart: TButton;
    btnTerminate: TButton;
    memLog: TMemo;
    btnDelete: TButton;
    ediStatus: TEdit;
    pnlTop: TPanel;
    pnlMain: TPanel;

    procedure btnStartClick(Sender: TObject);
    procedure btnTerminateClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);

  public
    FThread: ITaskAbstract;

    constructor Create(const AOwner: TComponent; AThread: ITaskAbstract); reintroduce;

    procedure Log(const AStr: string);
    procedure RefreshUI;
    procedure Terminate;
  end;

implementation

{$R *.dfm}

procedure TFrameThread.btnStartClick(Sender: TObject);
begin
  FThread.Run;
  RefreshUI;
end;

procedure TFrameThread.btnTerminateClick(Sender: TObject);
begin
  Terminate;
end;

procedure TFrameThread.btnDeleteClick(Sender: TObject);
begin
  Destroy;
end;

constructor TFrameThread.Create(const AOwner: TComponent; AThread: ITaskAbstract);
begin
  inherited Create(AOwner);

  btnDelete.Enabled := False;
  FThread := AThread;

  RefreshUI;
end;

procedure TFrameThread.Log(const AStr: string);
var
  LStr: string;
begin
  if Assigned(FThread) then
  begin
    LStr := FThread.GetName;
    memLog.Lines.Add(AStr);
  end
  else
    FGet.Fac.Log.Log(AStr);

  RefreshUI;
end;

procedure TFrameThread.RefreshUI;
begin
  // Exit;

  if not Assigned(FThread) then
    Exit;

  memLog.Perform(EM_SCROLL, SB_LINEDOWN, 0);

  ediStatus.Text := FThread.GetName + ': ';
  btnStart.Enabled := not FThread.IsStarted;

  if FThread.IsDone then
  begin
    ediStatus.Text := ediStatus.Text + 'Done';
    btnDelete.Enabled := True;
  end
  else
  begin
    if FThread.IsTerminated then
    begin
      ediStatus.Text := ediStatus.Text + 'Terminated';
    end
    else if FThread.IsStarted then
    begin
      ediStatus.Text := ediStatus.Text + 'Started';
    end;
  end;
end;

procedure TFrameThread.Terminate;
begin
  FThread.Terminate;
  RefreshUI;
end;

end.
