#!/usr/bin/env ruby
# frozen_string_literal: true

# InfoWorks ICM run script that always targets the latest network commit.
# It also validates scenarios before creating the run to avoid:
# "scenario was not validated so cannot be used in a run".

PROJECT_PATH = File.dirname(__FILE__)
DB_NAME = "your_database.icmm" # Change this to your database filename
DB_FILE = File.join(PROJECT_PATH, DB_NAME)

MODEL_GROUP_PATH_CANDIDATES = [
  ">MODG~Model group",
  ">MODG~Model Group"
].freeze

NETWORK_PATH_CANDIDATES = [
  ">MODG~Model group>NNET~Model network",
  ">MODG~Model Group>NNET~Model network"
].freeze

LEVEL_PATH_CANDIDATES = [
  ">MODG~Model group>LEV~Level",
  ">MODG~Model Group>LEV~Level"
].freeze
RAINFALL_EVENT_ID = 1

def fetch_latest_commit_id(network)
  return network.latest_commit_id if network.respond_to?(:latest_commit_id)
  return network.head_commit_id if network.respond_to?(:head_commit_id)
  return network.current_commit_id if network.respond_to?(:current_commit_id)
  return network.commit_id if network.respond_to?(:commit_id)

  if network.respond_to?(:commits)
    commits = network.commits
    latest = commits.respond_to?(:last) ? commits.last : nil
    return latest.id if latest&.respond_to?(:id)
    return latest.commit_id if latest&.respond_to?(:commit_id)
  end

  nil
end

def first_existing_model_object(db, candidates)
  candidates.each do |path|
    obj = db.model_object(path)
    return [obj, path] unless obj.nil?
  end
  [nil, nil]
end

def validate_base_scenario(network, scenario_name = "Base")
  validated = false

  if network.respond_to?(:validate_scenarios)
    network.validate_scenarios
    puts "Validated scenarios via network.validate_scenarios."
    validated = true
  end

  scenarios = []
  scenarios = network.scenarios.to_a if network.respond_to?(:scenarios)

  unless scenarios.empty?
    target = scenarios.find do |scenario|
      scenario.respond_to?(:name) &&
        scenario.name.to_s.casecmp(scenario_name).zero?
    end
    target ||= scenarios.first

    if target.respond_to?(:validate)
      target.validate
      scenario_label = target.respond_to?(:name) ? target.name : scenario_name
      puts "Validated scenario '#{scenario_label}'."
      validated = true
    end
  end

  validated
rescue StandardError => e
  puts "Scenario validation call failed: #{e.message}"
  false
end

def build_run_params(level_path)
  {
    "ExitOnFailedInit" => true,
    "Duration" => 14, # 14 hours
    "DurationUnit" => "Hours",
    "Level" => level_path,
    "ResultsMultiplier" => 300,
    "TimeStep" => 1,
    "StorePRN" => true,
    "DontLogModeSwitches" => false,
    "DontLogRTCRuleChanges" => false
  }
end

begin
  db = WSApplication.open(DB_FILE, false)
  raise "Could not open database: #{DB_FILE}" if db.nil?

  group, model_group_path = first_existing_model_object(db, MODEL_GROUP_PATH_CANDIDATES)
  raise "Model group not found. Tried: #{MODEL_GROUP_PATH_CANDIDATES.join(', ')}" if group.nil?

  network, network_path = first_existing_model_object(db, NETWORK_PATH_CANDIDATES)
  raise "Network not found. Tried: #{NETWORK_PATH_CANDIDATES.join(', ')}" if network.nil?

  _, level_path = first_existing_model_object(db, LEVEL_PATH_CANDIDATES)
  raise "Level object not found. Tried: #{LEVEL_PATH_CANDIDATES.join(', ')}" if level_path.nil?

  puts "Using model group path: #{model_group_path}"
  puts "Using network path: #{network_path}"
  puts "Using level path: #{level_path}"

  latest_commit_id = fetch_latest_commit_id(network)
  puts "Using latest network commit: #{latest_commit_id.nil? ? "default/latest" : latest_commit_id}"

  validate_base_scenario(network)

  run_name = "Run_Latest_#{Time.now.strftime('%Y%m%d_%H%M%S')}"
  run_params = build_run_params(level_path)

  begin
    run = group.new_run(
      run_name,
        network_path,
      latest_commit_id, # Explicit commit ID when available
      RAINFALL_EVENT_ID,
      "Auto-created from latest network commit",
      run_params
    )
  rescue StandardError => e
    if e.message =~ /scenario was not validated/i
      puts "Scenario validation required by ICM; retrying after validation."
      validate_base_scenario(network)
      run = group.new_run(
        run_name,
        network_path,
        latest_commit_id,
        RAINFALL_EVENT_ID,
        "Auto-created from latest network commit (retry)",
        run_params
      )
    else
      raise
    end
  end

  raise "Run creation failed." if run.nil?

  run_commit = if run.respond_to?(:commit_id)
                 run.commit_id
               elsif run.respond_to?(:commit)
                 run.commit
               else
                 "unknown"
               end
  puts "Created run '#{run_name}' on commit: #{run_commit}"

  sim = run.children[0]
  raise "No simulation child found for run '#{run_name}'." if sim.nil?

  WSApplication.connect_local_agent(1)
  WSApplication.launch_sims([sim], ".", false, 0, 0)

  loop do
    status = sim.status
    puts "Simulation status: #{status}"
    break unless ["None", "Queued", "Running"].include?(status)

    sleep 2
  end

  puts "Done."
rescue StandardError => e
  puts "Error: #{e.message}"
  puts e.backtrace
  raise
end
