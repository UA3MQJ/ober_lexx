defmodule OberLexx.Mixfile do
  use Mix.Project

  def project do
    [
      app: :oberlexx,
      version: "0.1.1",
      elixir: "~> 1.5",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      deps: deps(),
      compilers: [:yecc, :leex, :erlang] ++ Mix.compilers(),
      erlc_paths: ["priv"],  # Путь к вашему .xrl файлу
      compile_path: "priv",  # Путь для скомпилированных файлов
    ]
  end

  def application do
    [applications: [:logger],
     mod: {OberLexx, []}]
  end

  defp deps do
    [
      {:poison, "~> 3.1"},
    ]
  end
end
