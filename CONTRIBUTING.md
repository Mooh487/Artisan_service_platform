# Contributing to Artisan Service Platform

Thank you for your interest in contributing to the Artisan Service Platform! This document provides guidelines and instructions for contributing to this project.

## Code of Conduct

By participating in this project, you agree to abide by our Code of Conduct. Please be respectful and considerate of others.

## How to Contribute

### Reporting Bugs

If you find a bug, please create an issue with the following information:

- A clear, descriptive title
- Steps to reproduce the bug
- Expected behavior
- Actual behavior
- Screenshots (if applicable)
- Environment details (OS, browser, etc.)

### Suggesting Enhancements

We welcome suggestions for enhancements! Please create an issue with:

- A clear, descriptive title
- A detailed description of the proposed enhancement
- Any relevant examples or mockups
- Explanation of why this enhancement would be useful

### Pull Requests

1. Fork the repository
2. Create a new branch (`git checkout -b feature/your-feature-name`)
3. Make your changes
4. Run tests to ensure your changes don't break existing functionality
5. Commit your changes (`git commit -m 'Add some feature'`)
6. Push to the branch (`git push origin feature/your-feature-name`)
7. Create a new Pull Request

## Development Guidelines

### Clarity Smart Contract Development

When working on Clarity smart contracts:

1. Follow the [Clarity best practices](https://docs.stacks.co/write-smart-contracts/clarity-language/clarity-best-practices)
2. Ensure all functions have proper input validation
3. Use descriptive variable and function names
4. Add comments to explain complex logic
5. Run `clarinet check` to verify your contracts before submitting

### Testing

- Write tests for all new functionality
- Ensure all existing tests pass before submitting a PR
- Use the Clarinet testing framework for contract tests

### Documentation

- Update documentation to reflect any changes you make
- Document all public functions with clear descriptions
- Include examples where appropriate

## Git Workflow

1. Create a branch for your work
2. Make small, focused commits
3. Keep your PR focused on a single issue/feature
4. Rebase your branch on the latest master before submitting

## Review Process

All submissions require review. We use GitHub pull requests for this purpose.

1. A maintainer will review your PR
2. They may request changes or improvements
3. Once approved, your PR will be merged

## License

By contributing to this project, you agree that your contributions will be licensed under the project's MIT License.

Thank you for contributing to the Artisan Service Platform!
