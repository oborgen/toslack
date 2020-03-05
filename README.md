# toslack

Post Slack messages with this terminal tool.

## Prerequisites

* Stack

## Building

Run `stack build` inside the cloned repository.

## Installation

Run `stack install` when the program has been built and toslack will be available on the path.

## Usage

`toslack channel token`

`channel` is the Slack channel where the messages should be posted to.

`token` is the Slack authentication token for the app posting the messages.

Once the program has started, any lines passed into stdin will be posted to the specified Slack channel.
