#!/usr/bin/env python3
"""
Advent of Code Test Runner

This script runs all test suites and full runs for each year/day/language combination,
outputs a table showing pass/fail status, and compares outputs for consistency.
"""

import os
import subprocess
import json
from pathlib import Path
from collections import defaultdict
import sys
import shutil

class Colors:
    """ANSI color codes for terminal output"""
    GREEN = '\033[92m'
    RED = '\033[91m'
    YELLOW = '\033[93m'
    BLUE = '\033[94m'
    BOLD = '\033[1m'
    END = '\033[0m'

class TestRunner:
    def __init__(self):
        self.root_dir = Path.cwd()
        self.results = defaultdict(lambda: defaultdict(dict))
        self.outputs = defaultdict(lambda: defaultdict(dict))
        self.available_runtimes = self.check_language_runtimes()
        
    def find_years_and_days(self):
        """Find all year/day combinations in the project"""
        years_days = []
        for year_dir in self.root_dir.iterdir():
            if year_dir.is_dir() and year_dir.name.isdigit():
                for day_dir in year_dir.iterdir():
                    if day_dir.is_dir() and day_dir.name.startswith('day'):
                        years_days.append((year_dir.name, day_dir.name))
        return sorted(years_days)
    
    def check_language_runtimes(self):
        """Check which language runtimes are available on the system"""
        runtimes = {
            'python': ['python3'],
            'go': ['go'],
            'ruby': ['ruby'],
            'crystal': ['crystal'],
            'elixir': ['elixir'],
            'erlang': ['erl', 'erlc'],
            'haskell': ['ghc', 'runhaskell'],
            'lua': ['lua'],
            'forth': ['gforth']
        }
        
        available = {}
        for lang, commands in runtimes.items():
            available[lang] = all(shutil.which(cmd) is not None for cmd in commands)
        
        return available
    
    def find_languages(self, year, day):
        """Find all languages implemented for a given year/day"""
        day_path = self.root_dir / year / day
        languages = []
        
        # Look for language directories
        for item in day_path.iterdir():
            if item.is_dir() and item.name not in ['data']:
                # Check if it contains solution files
                if any(f.name.startswith('solution') for f in item.iterdir() if f.is_file()):
                    languages.append(item.name)
        
        return sorted(languages)
    
    def run_python_tests(self, year, day, lang_dir):
        """Run Python tests and solution"""
        results = {'tests': False, 'part_a': None, 'part_b': None}
        
        # Change to language directory
        original_cwd = os.getcwd()
        os.chdir(lang_dir)
        
        try:
            # Run tests
            test_files = list(lang_dir.glob('test*.py'))
            if test_files:
                for test_file in test_files:
                    try:
                        result = subprocess.run([sys.executable, test_file.name], 
                                              capture_output=True, text=True, timeout=30)
                        if result.returncode == 0:
                            results['tests'] = True
                    except subprocess.TimeoutExpired:
                        pass
            
            # Run solution with data files from ../data/
            solution_file = lang_dir / 'solution.py'
            if solution_file.exists():
                data_dir = lang_dir.parent / 'data'
                for input_file in ['input.txt', 'input-full.txt']:
                    input_path = data_dir / input_file
                    if input_path.exists():
                        try:
                            result = subprocess.run([sys.executable, 'solution.py', f'../data/{input_file}'], 
                                                  capture_output=True, text=True, timeout=30)
                            if result.returncode == 0:
                                output_lines = result.stdout.strip().split('\n')
                                if len(output_lines) >= 1:
                                    results['part_a'] = output_lines[0]
                                if len(output_lines) >= 2:
                                    results['part_b'] = output_lines[1]
                        except subprocess.TimeoutExpired:
                            pass
        finally:
            os.chdir(original_cwd)
        
        return results
    
    def run_go_tests(self, year, day, lang_dir):
        """Run Go tests and solution"""
        results = {'tests': False, 'part_a': None, 'part_b': None}
        
        original_cwd = os.getcwd()
        os.chdir(lang_dir)
        
        try:
            # Run tests
            try:
                result = subprocess.run(['go', 'test', '-v'], 
                                      capture_output=True, text=True, timeout=30)
                if result.returncode == 0:
                    results['tests'] = True
            except subprocess.TimeoutExpired:
                pass
            
            # Run solution with data files from ../data/
            data_dir = lang_dir.parent / 'data'
            for input_file in ['input.txt', 'input-full.txt']:
                input_path = data_dir / input_file
                if input_path.exists():
                    try:
                        result = subprocess.run(['go', 'run', 'solution.go', f'../data/{input_file}'], 
                                              capture_output=True, text=True, timeout=30)
                        if result.returncode == 0:
                            output_lines = result.stdout.strip().split('\n')
                            if len(output_lines) >= 1:
                                results['part_a'] = output_lines[0]
                            if len(output_lines) >= 2:
                                results['part_b'] = output_lines[1]
                    except subprocess.TimeoutExpired:
                        pass
        finally:
            os.chdir(original_cwd)
        
        return results
    
    def run_ruby_tests(self, year, day, lang_dir):
        """Run Ruby tests and solution"""
        results = {'tests': False, 'part_a': None, 'part_b': None}
        
        original_cwd = os.getcwd()
        os.chdir(lang_dir)
        
        try:
            # Run tests
            test_files = list(lang_dir.glob('test*.rb'))
            if test_files:
                try:
                    result = subprocess.run(['ruby', test_files[0].name], 
                                          capture_output=True, text=True, timeout=30)
                    if result.returncode == 0:
                        results['tests'] = True
                except subprocess.TimeoutExpired:
                    pass
            
            # Run solution with data files from ../data/
            solution_file = lang_dir / 'solution.rb'
            if solution_file.exists():
                data_dir = lang_dir.parent / 'data'
                for input_file in ['input.txt', 'input-full.txt']:
                    input_path = data_dir / input_file
                    if input_path.exists():
                        try:
                            result = subprocess.run(['ruby', 'solution.rb', f'../data/{input_file}'], 
                                                  capture_output=True, text=True, timeout=30)
                            if result.returncode == 0:
                                output_lines = result.stdout.strip().split('\n')
                                if len(output_lines) >= 1:
                                    results['part_a'] = output_lines[0]
                                if len(output_lines) >= 2:
                                    results['part_b'] = output_lines[1]
                        except subprocess.TimeoutExpired:
                            pass
        finally:
            os.chdir(original_cwd)
        
        return results
    
    def run_crystal_tests(self, year, day, lang_dir):
        """Run Crystal tests and solution"""
        results = {'tests': False, 'part_a': None, 'part_b': None}
        
        original_cwd = os.getcwd()
        os.chdir(lang_dir)
        
        try:
            # Run tests
            test_files = list(lang_dir.glob('test*.cr'))
            if test_files:
                try:
                    result = subprocess.run(['crystal', 'run', test_files[0].name], 
                                          capture_output=True, text=True, timeout=30)
                    if result.returncode == 0:
                        results['tests'] = True
                except subprocess.TimeoutExpired:
                    pass
            
            # Run solution with data files from ../data/
            solution_file = lang_dir / 'solution.cr'
            if solution_file.exists():
                data_dir = lang_dir.parent / 'data'
                for input_file in ['input.txt', 'input-full.txt']:
                    input_path = data_dir / input_file
                    if input_path.exists():
                        try:
                            result = subprocess.run(['crystal', 'run', 'solution.cr', '--', f'../data/{input_file}'], 
                                                  capture_output=True, text=True, timeout=30)
                            if result.returncode == 0:
                                output_lines = result.stdout.strip().split('\n')
                                if len(output_lines) >= 1:
                                    results['part_a'] = output_lines[0]
                                if len(output_lines) >= 2:
                                    results['part_b'] = output_lines[1]
                        except subprocess.TimeoutExpired:
                            pass
        finally:
            os.chdir(original_cwd)
        
        return results
    
    def run_elixir_tests(self, year, day, lang_dir):
        """Run Elixir tests and solution"""
        results = {'tests': False, 'part_a': None, 'part_b': None}
        
        original_cwd = os.getcwd()
        os.chdir(lang_dir)
        
        try:
            # Run tests
            test_files = list(lang_dir.glob('test*.exs'))
            if test_files:
                try:
                    result = subprocess.run(['elixir', test_files[0].name], 
                                          capture_output=True, text=True, timeout=30)
                    if result.returncode == 0:
                        results['tests'] = True
                except subprocess.TimeoutExpired:
                    pass
            
            # Run solution with data files from ../data/
            solution_file = lang_dir / 'solution.exs'
            if solution_file.exists():
                data_dir = lang_dir.parent / 'data'
                for input_file in ['input.txt', 'input-full.txt']:
                    input_path = data_dir / input_file
                    if input_path.exists():
                        try:
                            result = subprocess.run(['elixir', 'solution.exs', f'../data/{input_file}'], 
                                                  capture_output=True, text=True, timeout=30)
                            if result.returncode == 0:
                                output_lines = result.stdout.strip().split('\n')
                                if len(output_lines) >= 1:
                                    results['part_a'] = output_lines[0]
                                if len(output_lines) >= 2:
                                    results['part_b'] = output_lines[1]
                        except subprocess.TimeoutExpired:
                            pass
        finally:
            os.chdir(original_cwd)
        
        return results
    
    def run_erlang_tests(self, year, day, lang_dir):
        """Run Erlang tests and solution"""
        results = {'tests': False, 'part_a': None, 'part_b': None}
        
        original_cwd = os.getcwd()
        os.chdir(lang_dir)
        
        try:
            # Compile and run tests
            test_files = list(lang_dir.glob('test*.erl'))
            if test_files:
                try:
                    # Compile test
                    subprocess.run(['erlc', test_files[0].name], 
                                 capture_output=True, text=True, timeout=10)
                    # Run test
                    module_name = test_files[0].stem
                    result = subprocess.run(['erl', '-noshell', '-s', module_name, 'test', '-s', 'init', 'stop'], 
                                          capture_output=True, text=True, timeout=30)
                    if result.returncode == 0:
                        results['tests'] = True
                except subprocess.TimeoutExpired:
                    pass
            
            # Run solution with data files from ../data/
            solution_file = lang_dir / 'solution.erl'
            if solution_file.exists():
                try:
                    # Compile solution
                    subprocess.run(['erlc', 'solution.erl'], 
                                 capture_output=True, text=True, timeout=10)
                    
                    data_dir = lang_dir.parent / 'data'
                    for input_file in ['input.txt', 'input-full.txt']:
                        input_path = data_dir / input_file
                        if input_path.exists():
                            try:
                                result = subprocess.run(['erl', '-noshell', '-s', 'solution', 'main', f'../data/{input_file}', '-s', 'init', 'stop'], 
                                                      capture_output=True, text=True, timeout=30)
                                if result.returncode == 0:
                                    output_lines = result.stdout.strip().split('\n')
                                    if len(output_lines) >= 1:
                                        results['part_a'] = output_lines[0]
                                    if len(output_lines) >= 2:
                                        results['part_b'] = output_lines[1]
                            except subprocess.TimeoutExpired:
                                pass
                except subprocess.TimeoutExpired:
                    pass
        finally:
            os.chdir(original_cwd)
            # Clean up compiled files
            for f in lang_dir.glob('*.beam'):
                try:
                    f.unlink()
                except:
                    pass
        
        return results
    
    def run_haskell_tests(self, year, day, lang_dir):
        """Run Haskell tests and solution"""
        results = {'tests': False, 'part_a': None, 'part_b': None}
        
        original_cwd = os.getcwd()
        os.chdir(lang_dir)
        
        try:
            # Run tests
            test_files = list(lang_dir.glob('test*.hs'))
            if test_files:
                try:
                    # Try runhaskell first, then compile and run
                    result = subprocess.run(['runhaskell', test_files[0].name], 
                                          capture_output=True, text=True, timeout=30)
                    if result.returncode == 0:
                        results['tests'] = True
                    else:
                        # Try compiling and running
                        subprocess.run(['ghc', '-o', 'test_solution', test_files[0].name], 
                                     capture_output=True, text=True, timeout=30)
                        result = subprocess.run(['./test_solution'], 
                                              capture_output=True, text=True, timeout=30)
                        if result.returncode == 0:
                            results['tests'] = True
                except subprocess.TimeoutExpired:
                    pass
            
            # Run solution with data files from ../data/
            solution_file = lang_dir / 'solution.hs'
            if solution_file.exists():
                data_dir = lang_dir.parent / 'data'
                for input_file in ['input.txt', 'input-full.txt']:
                    input_path = data_dir / input_file
                    if input_path.exists():
                        try:
                            # Try runhaskell first
                            result = subprocess.run(['runhaskell', 'solution.hs', f'../data/{input_file}'], 
                                                  capture_output=True, text=True, timeout=30)
                            if result.returncode != 0:
                                # Try compiling and running
                                subprocess.run(['ghc', '-o', 'solution', 'solution.hs'], 
                                             capture_output=True, text=True, timeout=30)
                                result = subprocess.run(['./solution', f'../data/{input_file}'], 
                                                      capture_output=True, text=True, timeout=30)
                            
                            if result.returncode == 0:
                                output_lines = result.stdout.strip().split('\n')
                                if len(output_lines) >= 1:
                                    results['part_a'] = output_lines[0]
                                if len(output_lines) >= 2:
                                    results['part_b'] = output_lines[1]
                        except subprocess.TimeoutExpired:
                            pass
        finally:
            os.chdir(original_cwd)
            # Clean up compiled files
            for f in lang_dir.glob('solution'):
                try:
                    f.unlink()
                except:
                    pass
            for f in lang_dir.glob('test_solution'):
                try:
                    f.unlink()
                except:
                    pass
            for f in lang_dir.glob('*.hi'):
                try:
                    f.unlink()
                except:
                    pass
            for f in lang_dir.glob('*.o'):
                try:
                    f.unlink()
                except:
                    pass
        
        return results
    
    def run_lua_tests(self, year, day, lang_dir):
        """Run Lua tests and solution"""
        results = {'tests': False, 'part_a': None, 'part_b': None}
        
        original_cwd = os.getcwd()
        os.chdir(lang_dir)
        
        try:
            # Run tests
            test_files = list(lang_dir.glob('test*.lua'))
            if test_files:
                try:
                    result = subprocess.run(['lua', test_files[0].name], 
                                          capture_output=True, text=True, timeout=30)
                    if result.returncode == 0:
                        results['tests'] = True
                except subprocess.TimeoutExpired:
                    pass
            
            # Run solution with data files from ../data/
            solution_file = lang_dir / 'solution.lua'
            if solution_file.exists():
                data_dir = lang_dir.parent / 'data'
                for input_file in ['input.txt', 'input-full.txt']:
                    input_path = data_dir / input_file
                    if input_path.exists():
                        try:
                            result = subprocess.run(['lua', 'solution.lua', f'../data/{input_file}'], 
                                                  capture_output=True, text=True, timeout=30)
                            if result.returncode == 0:
                                output_lines = result.stdout.strip().split('\n')
                                if len(output_lines) >= 1:
                                    results['part_a'] = output_lines[0]
                                if len(output_lines) >= 2:
                                    results['part_b'] = output_lines[1]
                        except subprocess.TimeoutExpired:
                            pass
        finally:
            os.chdir(original_cwd)
        
        return results
    
    def run_language_tests(self, year, day, language):
        """Run tests for a specific language"""
        # Check if runtime is available
        if not self.available_runtimes.get(language, False):
            return {'tests': False, 'part_a': None, 'part_b': None, 'runtime_missing': True}
        
        lang_dir = self.root_dir / year / day / language
        
        if language == 'python':
            return self.run_python_tests(year, day, lang_dir)
        elif language == 'go':
            return self.run_go_tests(year, day, lang_dir)
        elif language == 'ruby':
            return self.run_ruby_tests(year, day, lang_dir)
        elif language == 'crystal':
            return self.run_crystal_tests(year, day, lang_dir)
        elif language == 'elixir':
            return self.run_elixir_tests(year, day, lang_dir)
        elif language == 'erlang':
            return self.run_erlang_tests(year, day, lang_dir)
        elif language == 'haskell':
            return self.run_haskell_tests(year, day, lang_dir)
        elif language == 'lua':
            return self.run_lua_tests(year, day, lang_dir)
        else:
            return {'tests': False, 'part_a': None, 'part_b': None}
    
    def check_consistency(self, year, day):
        """Check if outputs are consistent across languages"""
        outputs_a = []
        outputs_b = []
        
        for lang in self.outputs[year][day]:
            result = self.outputs[year][day][lang]
            if result['part_a'] is not None:
                outputs_a.append(result['part_a'])
            if result['part_b'] is not None:
                outputs_b.append(result['part_b'])
        
        consistent_a = len(set(outputs_a)) <= 1 if outputs_a else True
        consistent_b = len(set(outputs_b)) <= 1 if outputs_b else True
        
        return consistent_a and consistent_b
    
    def print_runtime_status(self):
        """Print the status of available language runtimes"""
        print(f"\n{Colors.BOLD}Language Runtime Status{Colors.END}")
        print("=" * 40)
        
        for language, available in self.available_runtimes.items():
            status = f"{Colors.GREEN}✓ Available{Colors.END}" if available else f"{Colors.YELLOW}✗ Not Found{Colors.END}"
            print(f"  {language.capitalize():<12} {status}")
    
    def run_all_tests(self):
        """Run all tests and collect results"""
        years_days = self.find_years_and_days()
        
        print(f"{Colors.BOLD}Running Advent of Code Test Suite{Colors.END}")
        print("=" * 60)
        
        # Print runtime availability
        self.print_runtime_status()
        
        for year, day in years_days:
            print(f"\n{Colors.BLUE}{Colors.BOLD}Testing {year}/{day}:{Colors.END}")
            languages = self.find_languages(year, day)
            
            for language in languages:
                print(f"  {language}... ", end="", flush=True)
                
                try:
                    result = self.run_language_tests(year, day, language)
                    self.results[year][day][language] = result
                    self.outputs[year][day][language] = result
                    
                    if result.get('runtime_missing', False):
                        print(f"{Colors.YELLOW}SKIP (runtime missing){Colors.END}")
                    else:
                        status = "✓" if result['tests'] else "✗"
                        color = Colors.GREEN if result['tests'] else Colors.RED
                        print(f"{color}{status}{Colors.END}")
                    
                except Exception as e:
                    print(f"{Colors.RED}ERROR{Colors.END}")
                    self.results[year][day][language] = {'tests': False, 'part_a': None, 'part_b': None}
                    self.outputs[year][day][language] = {'tests': False, 'part_a': None, 'part_b': None}
    
    def print_summary_table(self):
        """Print a summary table of all results"""
        print(f"\n\n{Colors.BOLD}Test Results Summary{Colors.END}")
        print("=" * 80)
        
        # Header
        header = f"{'Year/Day':<12} {'Language':<10} {'Tests':<8} {'Part A':<25} {'Part B':<25} {'Consistent':<10}"
        print(header)
        print("-" * 80)
        
        for year in sorted(self.results.keys()):
            for day in sorted(self.results[year].keys()):
                consistent = self.check_consistency(year, day)
                consistency_symbol = f"{Colors.GREEN}✓{Colors.END}" if consistent else f"{Colors.RED}✗{Colors.END}"
                
                for i, language in enumerate(sorted(self.results[year][day].keys())):
                    result = self.results[year][day][language]
                    
                    # Format test result
                    if result.get('runtime_missing', False):
                        test_status = f"{Colors.YELLOW}SKIP{Colors.END}"
                    else:
                        test_status = f"{Colors.GREEN}PASS{Colors.END}" if result['tests'] else f"{Colors.RED}FAIL{Colors.END}"
                    
                    # Format outputs (truncate if too long)
                    part_a = str(result['part_a']) if result['part_a'] is not None else "N/A"
                    part_b = str(result['part_b']) if result['part_b'] is not None else "N/A"
                    
                    if len(part_a) > 20:
                        part_a = part_a[:17] + "..."
                    if len(part_b) > 20:
                        part_b = part_b[:17] + "..."
                    
                    # Show year/day only for first language
                    year_day = f"{year}/{day}" if i == 0 else ""
                    consistency_col = consistency_symbol if i == 0 else ""
                    
                    print(f"{year_day:<12} {language:<10} {test_status:<15} {part_a:<25} {part_b:<25} {consistency_col:<10}")
    
    def print_detailed_outputs(self):
        """Print detailed outputs for consistency checking"""
        print(f"\n\n{Colors.BOLD}Detailed Output Comparison{Colors.END}")
        print("=" * 60)
        
        for year in sorted(self.outputs.keys()):
            for day in sorted(self.outputs[year].keys()):
                print(f"\n{Colors.BLUE}{Colors.BOLD}{year}/{day}:{Colors.END}")
                
                # Group outputs by part
                part_a_outputs = {}
                part_b_outputs = {}
                
                for language in sorted(self.outputs[year][day].keys()):
                    result = self.outputs[year][day][language]
                    if result['part_a'] is not None:
                        part_a_outputs[language] = result['part_a']
                    if result['part_b'] is not None:
                        part_b_outputs[language] = result['part_b']
                
                # Check Part A consistency
                if part_a_outputs:
                    unique_a = set(part_a_outputs.values())
                    consistent_a = len(unique_a) <= 1
                    status_a = f"{Colors.GREEN}CONSISTENT{Colors.END}" if consistent_a else f"{Colors.RED}INCONSISTENT{Colors.END}"
                    
                    print(f"  Part A ({status_a}):")
                    for lang, output in part_a_outputs.items():
                        print(f"    {lang}: {output}")
                
                # Check Part B consistency
                if part_b_outputs:
                    unique_b = set(part_b_outputs.values())
                    consistent_b = len(unique_b) <= 1
                    status_b = f"{Colors.GREEN}CONSISTENT{Colors.END}" if consistent_b else f"{Colors.RED}INCONSISTENT{Colors.END}"
                    
                    print(f"  Part B ({status_b}):")
                    for lang, output in part_b_outputs.items():
                        print(f"    {lang}: {output}")

def main():
    """Main function"""
    runner = TestRunner()
    
    try:
        runner.run_all_tests()
        runner.print_summary_table()
        runner.print_detailed_outputs()
        
        print(f"\n{Colors.BOLD}Test run complete!{Colors.END}")
        
    except KeyboardInterrupt:
        print(f"\n{Colors.YELLOW}Test run interrupted by user{Colors.END}")
    except Exception as e:
        print(f"\n{Colors.RED}Error during test run: {e}{Colors.END}")
        return 1
    
    return 0

if __name__ == "__main__":
    sys.exit(main())